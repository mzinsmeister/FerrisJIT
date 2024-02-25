mod expr;
mod codegen;

use std::{error::Error, fmt::Display, hint::black_box};

use codegen::{generate_code, STENCILS};
use expr::parse_expr_from_str;


use clap::Parser;
use csv::Reader;
use rustyline::{error::ReadlineError, history::MemHistory, Config, Editor};

use crate::{codegen::ir::DataType, expr::eval_expression};

/// Search for a pattern in a file and display the lines that contain it.
#[derive(Parser)]
struct Cli {
    /// The path to the file to read
    #[arg(short, long)]
    csv: Option<std::path::PathBuf>,
    /// Whether to run in benchmark mode
    #[arg(short, long)]
    benchmark: bool,
}

fn eval<T: Display>(expr: &expr::Expr, test_data: &[Vec<i64>], benchmark: bool) {
    let codegen_start = std::time::Instant::now();
    let code = generate_code::<T>(&expr, test_data[0].len());
    let codegen_elapsed = codegen_start.elapsed();
    match code {
        Ok(code) => {
            println!("Generated {} bytes of x86-64 binary in {:?}", code.code_len, codegen_elapsed);
            if benchmark {
                let start_time = std::time::Instant::now();
                for line in test_data.iter() {
                    code.call(line);
                }
                
                let elapsed = start_time.elapsed();

                
                let start_interp = std::time::Instant::now();

                for line in test_data.iter() {

                    let interp_result = eval_expression(&expr, line);

                    black_box(interp_result);

                }
                let elapsed_interp = start_interp.elapsed();

                //let start_hardcoded = std::time::Instant::now();
                //--- INSERT YOUR HARDCODED EXPRESSION EVALUATION HERE ---
                //let elapsed_hardcoded = start_hardcoded.elapsed();
                //println!("Hardcoded: {:?}", elapsed_hardcoded);
                
                println!("Interpreted: {:?}", elapsed_interp);
                println!("Compiled: {:?}", elapsed);

                let factor = elapsed_interp.as_secs_f64() / elapsed.as_secs_f64();

                println!("Compiled is {:.2}x {}", factor, if factor > 1.0 { "faster" } else { "slower" });

            } else {
                for line in test_data.iter() {
                    let result = code.call(line);
                    println!("Result: {}", result);
                }
            }
        },
        Err(c) => {
            match c {
                codegen::CodeGenError::TypeError => {
                    println!("Type error");
                }
                codegen::CodeGenError::Const(n) => {
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

    let test_data: Vec<Vec<i64>> = if let Some(csv_path) = args.csv {
        println!("Reading data from csv file: {:?}", csv_path);
        let mut rdr = Reader::from_path(csv_path)?;
        rdr.records()
            .map(|record| {
                let record = record.unwrap();
                let mut vec = Vec::new();
                for field in record.iter() {
                    vec.push(field.parse::<i64>().unwrap());
                }
                vec
            })
            .collect()
    } else {
        println!("No csv file specified, using default dummy data");
        if args.benchmark {
            (0..1_000_000i64).map(|n| vec![n]).collect()
        } else {
            (0..10).map(|n| vec![n]).collect()
        }
    };

    if test_data.len() == 0 {
        println!("No data to process");
        return Ok(());
    }

    // Dummy access to initialize stencils
    let compile_start = std::time::Instant::now();
    black_box(STENCILS.len());
    let compile_elapsed = compile_start.elapsed();
    println!("Stencil initialization: {:?}", compile_elapsed);

    // REPL for evaluating expressions on the data

    let mut rl = Editor::<(), MemHistory>::with_history(Config::default(), MemHistory::new())?;


    loop {
        let readline = rl.readline(">> ");
        match readline {
            Ok(line) => {
                rl.add_history_entry(line.as_str()).unwrap();
                let expr = parse_expr_from_str(&line);
                let expr = match expr {
                    Ok(expr) => expr,
                    Err(e) => {
                        println!("Parse-Error: {}", e);
                        continue;
                    }
                };     
                let result_type = codegen::get_type(&expr);

                match result_type {
                    DataType::I64 => eval::<i64>(&expr, &test_data, args.benchmark),
                    DataType::Bool => eval::<bool>(&expr, &test_data, args.benchmark),
                    _ => unreachable!()
                }            
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

    use self::expr::Atom;

    use super::*;

    #[test]
    fn test_codegen_1() {
        let expr_str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 4) $0)))";
        let expr = parse_expr_from_str(expr_str).unwrap();
        let code = generate_code::<i64>(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]).unwrap();
            if let Atom::Num(n) = interp_result {
                assert_eq!(result, n);
            } else {
                panic!("Datatype missmatch");
            }
        }
    }

    #[test]
    fn test_codegen_2() {
        let expr_str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 (+ 1 4)) $0)))";
        let expr = parse_expr_from_str(expr_str).unwrap();
        let code = generate_code::<i64>(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]).unwrap();
            if let Atom::Num(n) = interp_result {
                assert_eq!(result, n);
            } else {
                panic!("Datatype missmatch");
            }
        }
    }

    #[test]
    fn test_codegen_3() {
        let expr_str = "(+ (+ $0 $0) (* (/ $0 2) (- (* 9 4) $0)))";

        let expr = parse_expr_from_str(expr_str).unwrap();
        let code = generate_code::<i64>(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]).unwrap();
            if let Atom::Num(n) = interp_result {
                assert_eq!(result, n);
            } else {
                panic!("Datatype missmatch");
            }
        }
    }


    #[test]
    fn test_codegen_4() {
        let expr_str = "(= (= $0 5) (= (- (+ $0 $0) (* -2 $0)) 20)))";
        let expr = parse_expr_from_str(expr_str).unwrap();
        let code = generate_code::<bool>(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]).unwrap();
            if let Atom::Boolean(n) = interp_result {
                assert_eq!(result, n);
            } else {
                panic!("Datatype missmatch");
            }
        }
    }

    const VERY_COMPLEX_EXPR_1: &str = include_str!("complex_expr.txt");

    #[test]
    fn test_codegen_very_complex_1() {
        let expr = parse_expr_from_str(VERY_COMPLEX_EXPR_1).unwrap();
        let code = generate_code::<i64>(&expr, 1).unwrap();
        for i in [0, 1, 5].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]).unwrap();
            if let Atom::Num(n) = interp_result {
                assert_eq!(result, n);
            } else {
                panic!("Datatype missmatch");
            }
        }
    }
}