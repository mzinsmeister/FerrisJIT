mod query;
mod codegen;
mod query_codegen;

use std::{error::Error, hint::black_box, ptr, time::Duration};

use codegen::CodeGenContext;
use query_codegen::generate_code;



use clap::Parser;
use csv::Reader;
use rustyline::{error::ReadlineError, history::MemHistory, Config, Editor};

use crate::query::{parse_query_from_str, run_query};

// Empty consumer for benchmarking
unsafe extern "C" fn noop_result_consumer(_: *mut u8, _: *mut u8, _: *mut u8) -> *mut u8 {
    return ptr::null_mut();
}

unsafe extern "C" fn stdout_result_consumer(_: *mut u8, args: *mut u8, _: *mut u8) -> *mut u8 {
    // Since we only have a single argument, we passed it by value
    let result = args as i64; 
    println!("Result: {}", result);
    return ptr::null_mut();
}

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    csv: Option<std::path::PathBuf>,
    /// Whether to run in benchmark mode
    #[arg(short, long)]
    benchmark: bool,
    /// Number of elements to generate
    #[arg(short, long)]
    number: Option<u64>
}

fn format_time_comparison(duration1: Duration, duration2: Duration) -> String {
    if duration1 > duration2 {
        let factor = duration1.as_secs_f64() / duration2.as_secs_f64();
        format!("{:.2}x slower", factor)
    } else if duration2 > duration1 {
        let factor = duration2.as_secs_f64() / duration1.as_secs_f64();
        format!("{:.2}x faster", factor)
    } else {
        "equaly fast".to_string()
    
    }
}

const LLVM_LEVELS: [(inkwell::OptimizationLevel, &str); 2] = [(inkwell::OptimizationLevel::None, "O0"), (inkwell::OptimizationLevel::Aggressive, "O3")];

fn eval(query: &query::Query, test_data: (usize, &[i64]), benchmark: bool) {
    let codegen_start = std::time::Instant::now();
    let result_consumer = if benchmark {
        noop_result_consumer
    } else {
        stdout_result_consumer
    };
    let mut context = CodeGenContext::new();
    // Generate x86 through the Copy&Patch backend and LLVM-IR in a single go
    let cgresult = generate_code(&mut context, &query, test_data.0, result_consumer);
    let codegen_elapsed = codegen_start.elapsed();
    match cgresult {
        Ok(cgresult) => {
            println!("Generated {} bytes of x86-64 binary (+ equivalent LLVM-IR) in {:?}", cgresult.code.code_len, codegen_elapsed);
            // Now we compile the LLVM-IR to machine code, first with optimization "None" (-O0), then with "Aggressive" (-03)

            if benchmark {
                let start_time = std::time::Instant::now();
                cgresult.code.call(&[test_data.1.as_ptr() as usize, (test_data.1.len() / test_data.0) as usize]);
                let elapsed = start_time.elapsed();
                
                let start_interp = std::time::Instant::now();

                run_query(&query, test_data.1, test_data.0, |result| {black_box(result);});
                
                let elapsed_interp = start_interp.elapsed();

                let mut llvm_results = Vec::new();



                for (level, name) in  LLVM_LEVELS.iter() {
                    let compile_start_time = std::time::Instant::now();
                    let llvm_func = cgresult.compile_llvm(*level);
                    let compile_elapsed = compile_start_time.elapsed();
                    let exec_start_time = std::time::Instant::now();
                    llvm_func.call(&[test_data.1.as_ptr() as usize, (test_data.1.len() / test_data.0) as usize]);
                    let exec_elapsed = exec_start_time.elapsed();
                    llvm_results.push((name, compile_elapsed, exec_elapsed));
                }


                //let start_hardcoded = std::time::Instant::now();
                //--- INSERT YOUR HARDCODED EXPRESSION EVALUATION HERE ---
                //let elapsed_hardcoded = start_hardcoded.elapsed();
                //println!("Hardcoded: {:?}", elapsed_hardcoded);
                
                println!("Interpreted: {:?}", elapsed_interp);
                println!("Compiled: {:?}", elapsed);


                println!("Compiled is {}", format_time_comparison(elapsed, elapsed_interp));

                println!("LLVM Results: ");
                for (name, compile_elapsed, exec_elapsed) in llvm_results {
                    println!("{}: Compile: {:?}({} than C&P), Exec: {:?} ({} than C&P)", name, compile_elapsed, 
                        format_time_comparison(compile_elapsed, codegen_elapsed), exec_elapsed, format_time_comparison(exec_elapsed, elapsed));
                }
            } else {
                cgresult.code.call(&[test_data.1.as_ptr() as usize, (test_data.1.len() / test_data.0) as usize]);
            }
        },
        Err(c) => {
            match c {
                query_codegen::CodeGenError::TypeError => {
                    println!("Type error");
                }
                query_codegen::CodeGenError::Const(n) => {
                    println!("Result(const): {}", n);
                }
            }
        }
    }
}


fn main() -> Result<(), Box<dyn Error>> {

    let args = Cli::parse();

    // Fill a test vector with 100_000_000 elements
    // If the user specified a csv (no header, comma separated) file we will read the data from there
    // otherwise we will generate a sequential range of numbers 0..10_000_000

    let test_data: (usize, Vec<i64>) = if let Some(csv_path) = args.csv {
        println!("Reading data from csv file: {:?}", csv_path);
        let mut rdr = Reader::from_path(csv_path)?;
        let rows: Vec<Vec<i64>> = rdr.records()
            .map(|record| {
                let record = record.unwrap();
                let mut vec = Vec::new();
                for field in record.iter() {
                    vec.push(field.parse::<i64>().unwrap());
                }
                vec
            })
            .collect();
        if rows.len() == 0 {
            println!("No data to process");
            return Ok(());
        }
        let n = rows[0].len();
        (n, rows.concat())
    } else {
        println!("No csv file specified, using default dummy data");
        let default_n = if args.benchmark {
            1_000_000i64
        } else {
            10
        };
        if let Some(0) = args.number {
            println!("Number of elements must be greater than 0");
            return Ok(());
        }
        let n = args.number.map_or(default_n, |n| n as i64);
        (1, (0..n).collect())
    };

    codegen::init_stencils();

    // REPL for evaluating expressions on the data
    let mut rl = Editor::<(), MemHistory>::with_history(Config::default(), MemHistory::new())?;


    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                let parse_start = std::time::Instant::now();
                let query = parse_query_from_str(&line);
                let query = match query {
                    Ok(expr) => expr,
                    Err(e) => {
                        println!("Parse-Error: {}", e);
                        continue;
                    }
                };
                let parse_elapsed = parse_start.elapsed();
                println!("Parsed in {:?}", parse_elapsed);

                eval(&query, (test_data.0, &test_data.1), args.benchmark)
            },
            Err(ReadlineError::Interrupted) => {
                println!("CTRL-C");
                break
            },
            Err(ReadlineError::Eof) => {
                println!("CTRL-D");
                break
            },
            Err(err) => {
                println!("Error: {:?}", err);
                break
            }
        }
    }

    Ok(())
}

#[cfg(test)]
mod test {
    use crate::{codegen::CodeGenContext, query::{parse_query_from_str, run_query, Atom}, query_codegen::generate_code, test::results::Results};

    mod results {
        use std::{cell::RefCell, mem, ops::DerefMut, ptr};


        // This is a hack for testing. We definitely want to handle results differently in a real system
        thread_local! {
            static RESULTS: RefCell<Vec<i64>> = RefCell::new(vec![]);
        }

        pub struct Results();

        impl Drop for Results {
            fn drop(&mut self) {
                RESULTS.with_borrow_mut(|r| r.clear())
            }
        }

        impl Results {

            pub fn take(&self) -> Vec<i64> {
                RESULTS.with(|r: &RefCell<Vec<i64>>| {
                    let mut result = vec![];
                    let mut r_mut = r.borrow_mut();
                    mem::swap(r_mut.deref_mut(), &mut result);
                    result
                })
            }

            pub fn consumer(&self) -> unsafe extern "C" fn(*mut u8, *mut u8, *mut u8) -> *mut u8 {
                test_result_consumer
            }
        }

        unsafe extern "C" fn test_result_consumer(_: *mut u8, args: *mut u8, _: *mut u8) -> *mut u8 {
            // Since we only have a single argument, we passed it by value
            let result = args as i64; 
            RESULTS.with(|r| {
                r.borrow_mut().push(result);
            });
            ptr::null_mut()
        }    

    }

    #[test]
    fn test_codegen_0() {
        let results = Results();
        let expr_str = "(+ $0 1)";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0i64, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }
    
    #[test]
    fn test_codegen_1() {
        let results = Results();
        let expr_str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 4) $0)))";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0i64, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    #[test]
    fn test_codegen_2() {
        let results = Results();
        let expr_str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 (+ 1 4)) $0)))";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0i64, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    #[test]
    fn test_codegen_3() {
        let results = Results();
        let expr_str = "(+ (+ $0 $0) (* (/ $0 2) (- (* 9 4) $0)))";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    // TODO: Create at least some basic tests for aggregates and filters

    #[test]
    fn test_codegen_4() {
        let results = Results();
        let expr_str = "(- (/ $0 2) (* -2 $0))";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    #[test]
    fn test_codegen_sum() {
        let results = Results();
        let expr_str = "SUM $0";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    #[test]
    fn test_codegen_where() {
        let results = Results();
        let expr_str = "$0 WHERE (> $0 10)";
        let query = parse_query_from_str(expr_str).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0, 1, 5, 10, 100, 1000];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }

    const VERY_COMPLEX_EXPR_1: &str = include_str!("complex_expr.txt");

    #[test]
    fn test_codegen_very_complex_1() {
        let results = Results();
        let query = parse_query_from_str(VERY_COMPLEX_EXPR_1).unwrap();
        let mut ctx = CodeGenContext::new();
        let code = generate_code(&mut ctx, &query, 1, results.consumer()).unwrap();
        let data = vec![0, 1, 5];
        code.code.call(&[data.as_ptr() as usize, data.len()]);
        let mut interp_result = vec![];
        run_query(&query, &data, 1, |r| match r {
            Atom::Num(n) => interp_result.push(n),
            _ => unreachable!()
        });
        let result = results.take();
        assert_eq!(result, interp_result);

        // Compile LLVM
        let llvm_func = code.compile_llvm(inkwell::OptimizationLevel::None);
        llvm_func.call(&[data.as_ptr() as usize, data.len()]);
        let result = results.take();
        assert_eq!(result, interp_result);
    }
}