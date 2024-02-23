mod expr;
mod codegen;

use std::{error::Error, hint::black_box};

use codegen::{generate_code, STENCILS};
use expr::parse_expr_from_str;


use clap::Parser;
use csv::Reader;
use rustyline::{error::ReadlineError, history::MemHistory, Config, Editor};

use crate::expr::eval_expression;

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
                let codegen_start = std::time::Instant::now();
                let code = generate_code(&expr, test_data[0].len());
                let codegen_elapsed = codegen_start.elapsed();
                println!("Codegen+Compile: {:?}", codegen_elapsed);
                match code {
                    Ok(code) => {
                        if args.benchmark {
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


                        } else {
                            for line in test_data.iter() {
                                let result = code.call(line);
                                println!("Result: {}", result);
                            }
                        }     
                    },
                    Err(c) => {
                        println!("Result (const): {}", c);
                    }
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

    use super::*;

    //const complex_expr: &str = include_str!("./complex_expr.txt");

    const COMPLEX_EXPR: &str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 4) $0)))";

    #[test]
    fn test_codegen1() {
        let expr = parse_expr_from_str(COMPLEX_EXPR).unwrap();
        let code = generate_code(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]);
            assert_eq!(result, interp_result);
        }
    }

    const COMPLEX_EXPR_2: &str = "(+ (+ $0 $0) (* (+ $0 $0) (+ (* 9 (+ 1 4)) $0)))";

    #[test]
    fn test_codegen2() {
        let expr = parse_expr_from_str(COMPLEX_EXPR_2).unwrap();
        let code = generate_code(&expr, 1).unwrap();
        for i in [0, 1, 5, 10, 100, 1000].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]);
            assert_eq!(result, interp_result);
        }
    }

    const VERY_COMPLEX_EXPR_1: &str = include_str!("complex_expr.txt");

    #[test]
    fn test_codegen3_very_complex() {
        let expr = parse_expr_from_str(VERY_COMPLEX_EXPR_1).unwrap();
        let code = generate_code(&expr, 1).unwrap();
        for i in [0, 1, 5].iter() {
            let result = code.call(&[*i]);
            let interp_result = eval_expression(&expr, &[*i]);
            assert_eq!(result, interp_result);
        }
    }
}