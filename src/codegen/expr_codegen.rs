use std::fmt::Display;

use crate::expr::{Atom, BuiltIn, Expr};

#[cfg(feature = "print-asm")]
use crate::codegen::disassemble;

use super::{ir::DataType, BoolRef, CGEq, CGValueRef, CodeGen, GeneratedCode, I64Ref};

fn fold_op(fun: &BuiltIn, l: Atom, r: Atom) -> Option<Atom> {
    match (fun, l, r) {
        (BuiltIn::Plus, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l + r)),
        (BuiltIn::Times, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l * r)),
        (BuiltIn::Minus, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l - r)),
        (BuiltIn::Divide, Atom::Num(l), Atom::Num(r)) => Some(Atom::Num(l / r)),
        (BuiltIn::Equal, Atom::Num(l), Atom::Num(r)) => Some(Atom::Boolean(l == r)),
        (BuiltIn::Equal, Atom::Boolean(l), Atom::Boolean(r)) => Some(Atom::Boolean(l == r)),
        _ => None,
    }
}

fn is_commutative(fun: &BuiltIn) -> bool {
    match fun {
        BuiltIn::Plus | BuiltIn::Times | BuiltIn::Equal => true,
        _ => false,
    }
}

pub fn get_type(expr: &Expr) -> DataType {
    match expr {
        Expr::Constant(Atom::Num(_)) => DataType::I64,
        Expr::Constant(Atom::Boolean(_)) => DataType::Bool,
        Expr::Variable(_) => DataType::I64,
        Expr::Application(fun, _) => {
            match fun {
                BuiltIn::Plus | BuiltIn::Minus | BuiltIn::Times | BuiltIn::Divide => {
                    DataType::I64
                },
                BuiltIn::Equal => {
                    DataType::Bool
                }
            }
        },
    }
}

fn fold_constants(fun: &BuiltIn, args: &[Expr]) -> Option<Vec<Expr>> {
    if is_commutative(fun) {
        fold_all_constants_commutative(fun, args)
    } else {
        // We just merge all the constants that appear in the beginning of the list for now which should be safe to do
        // assuming that a list of arguments just means folding the arguments using the function
        let mut result = Vec::new();

        let mut args_iter = args.iter().peekable();

        let data_type = get_type(&args[0]);
        
        if let Some(Expr::Constant(n)) = args_iter.peek() {
            let mut folded_constants = *n;
            args_iter.next();
            while let Some(Expr::Constant(n)) = args_iter.peek() {
                folded_constants = fold_op(fun, folded_constants, *n)?;
                args_iter.next();
            }
            result.push(Expr::Constant(folded_constants));
        }

        for arg in args_iter {
            if get_type(&arg) != data_type {
                return None;
            }
            match arg {
                Expr::Application(fun2, s) => {
                    let folded_s = fold_constants(fun2, s)?;
                    if folded_s.len() == 1 {
                        result.push(folded_s[0].clone());
                    } else {
                        result.push(Expr::Application(*fun2, folded_s));
                    }
                },
                _ => result.push(arg.clone()),
            }
        }

        Some(result)
    }
}

fn fold_all_constants_commutative(fun: &BuiltIn, args: &[Expr]) -> Option<Vec<Expr>> {
    let mut applications = Vec::new();
    let mut variables = Vec::new();
    let mut folded_constants = None;

    let data_type = get_type(&args[0]);

    // Add all the variables to result and fold all constants into one
    for arg in args {
        if get_type(&arg) != data_type {
            return None;
        }
        match arg {
            Expr::Constant(n) => {
                if let Some(folded_constants_n) = folded_constants {
                    folded_constants = Some(fold_op(fun, folded_constants_n, *n)?);
                } else {
                    folded_constants = Some(*n);
                }
            },
            Expr::Variable(n) => {
                variables.push(Expr::Variable(*n));
            },
            Expr::Application(fun2, s) => {
                let folded_s = fold_constants(fun2, s)?;
                if folded_s.len() == 1 {
                    match folded_s[0] {
                        Expr::Constant(n) => {
                            if let Some(folded_constants_n) = folded_constants {
                                folded_constants = Some(fold_op(fun, folded_constants_n, n)?);
                            } else {
                                folded_constants = Some(n);
                            }
                        },
                        Expr::Variable(n) => {
                            variables.push(Expr::Variable(n));
                        },
                        _ => unreachable!()
                    }
                } else {
                    applications.push(Expr::Application(*fun2, folded_s));
                }
            },
        }
    }

    // We make sure to execute applications first then variables so that
    // we minimize the number of times we need to take from the stack
    let mut result = Vec::new();
    result.extend(applications);
    result.extend(variables);

    if let Some(folded_constants_n) = folded_constants {
        result.push(Expr::Constant(folded_constants_n));
    }

    Some(result)
}

#[derive(Debug)]
pub enum CodeGenError {
    Const(Atom),
    TypeError
}

impl Display for CodeGenError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            CodeGenError::Const(n) => {
                write!(f, "Result(const): {}", n)
            },
            CodeGenError::TypeError => {
                write!(f, "Type error")
            }
        }
    }
}

fn generate_atom<'cg>(cg: &'cg CodeGen, atom: &Atom) -> CGValueRef<'cg> {
    match atom {
        Atom::Num(n) => {
            cg.new_i64_const(*n).into()
        },
        Atom::Boolean(b) => {
            cg.new_bool_const(*b).into()
        },
    }
}

fn generate_int_op<'cg>(_cg: &'cg CodeGen, fun: &BuiltIn, left: I64Ref<'cg>, right: I64Ref<'cg>) -> CGValueRef<'cg> {
    match fun {
        BuiltIn::Plus => {
            (left + &right).into()
        },
        BuiltIn::Times => {
            (left * &right).into()
        },
        BuiltIn::Minus => {
            (left - &right).into()
        },
        BuiltIn::Divide => {
            (left / &right).into()
        },
        BuiltIn::Equal => {
            left.cg_eq(&right).into()
        }
    }
}

fn generate_bool_op<'cg>(_cg: &'cg CodeGen, fun: &BuiltIn, left: BoolRef<'cg>, right: BoolRef<'cg>) -> BoolRef<'cg> {
    match fun {
        BuiltIn::Equal => {
            left.cg_eq(&right)
        }
        _ => todo!("For the moment no bools")
    }
}

fn generate_code_application<'cg>(cg: &'cg CodeGen, fun: &BuiltIn, args: &[Expr]) -> Result<CGValueRef<'cg>, CodeGenError> {
    let first_variable = &args[0];

    let mut cur = match first_variable {
        Expr::Variable(n) => {
            cg.get_arg(*n).into()
        },
        Expr::Constant(n) => {
            generate_atom(cg, n)
        },
        Expr::Application(fun2, args2) => {
            generate_code_application(cg,&fun2, &args2)?
        },
    };

    for arg in args.iter().skip(1) {
        let next: CGValueRef<'cg> = match arg {
            Expr::Variable(n) => {
                cg.get_arg(*n).into()
            },
            Expr::Constant(n) => {
                generate_atom(cg, n)
            },
            Expr::Application(fun2, args2) => {
                // Save the current result to the stack 
                let folded_args = if let Some(fa) = fold_constants(fun2, args2) {
                    fa
                } else {
                    return Err(CodeGenError::TypeError);
                };
                generate_code_application(cg, &fun2, &folded_args)?
            },
        };
        match cur.data_type {
            DataType::I64 => {
                let left = I64Ref::from(cur);
                let right = I64Ref::from(next);
                cur = generate_int_op(cg, fun, left, right).into();
            },
            DataType::Bool => {
                let left = BoolRef::from(cur);
                let right = BoolRef::from(next);
                cur = generate_bool_op(cg, fun, left, right).into();
            },
            _ => todo!("For the moment only int64s and bools")
        }
    }
    Ok(cur)
}

fn generate_code_inner<'cg>(cg: &'cg CodeGen, expr: &Expr) -> Result<CGValueRef<'cg>, CodeGenError> {
    Ok(match expr {
        Expr::Constant(e) => {
            return Err(CodeGenError::Const(e.clone()));
        },
        Expr::Variable(n) => {
            cg.get_arg(*n).into()
        },
        Expr::Application(fun, args) => {
            let folded_args = if let Some(fa) = fold_constants(fun, args) {
                fa
            } else {
                return Err(CodeGenError::TypeError);
            };
            if folded_args.len() == 1 {
                match &folded_args[0] {
                    // TODO: Once we introduce operators that also do something to
                    //       a single argument, this is no longer correct like this
                    //       but constant folding will need to be imroved anyway then
                    Expr::Constant(n) => {
                        return Err(CodeGenError::Const(n.clone()));
                    },
                    Expr::Variable(n) => {
                        cg.get_arg(*n).into()
                    },
                    _ => {
                        generate_code_application(cg, fun, &folded_args)?
                    },
                }
            } else {
                generate_code_application(cg, fun, &folded_args)?
            }
        },
    })
}

pub fn generate_code(expr: &Expr, args: usize) -> Result<GeneratedCode, CodeGenError> {

    let cg = CodeGen::new(args);

    // Uncomment this and comment the gnerate_code_inner to test ifs
    
    //let c1 = cg.new_i64_const(0);
    //let c2 = cg.new_i64_const(1);
    //let b1 = c1.cg_eq(&c2);

    let b1 = cg.new_bool_const(true);

    /*let mut val = cg.new_i64_const(0);

    cg.generate_if(b1, || {
        val = val.clone() + &cg.new_i64_const(1);
    });*/

    let return_value = b1.into(); // val.into();
    

    //let return_value = generate_code_inner(&cg, expr)?;

    cg.generate_return(return_value);

    let gc = cg.generate_code();

    Ok(gc)
}
