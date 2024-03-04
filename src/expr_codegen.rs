use std::{fmt::Display, ops::Deref};

use crate::{codegen::{CGCmp, CodegenCFunctionSignature, PtrRefByteOffset, Setable, TypedPtrRef, TypedPtrRefOffset, UntypedPtrRef}, expr::{Atom, BuiltIn, Expr}};

#[cfg(feature = "print-asm")]
use crate::codegen::disassemble;

use crate::codegen::{ir::DataType, BoolRef, CGEq, CGValueRef, CodeGen, GeneratedCode, I64Ref};

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

fn generate_code_application<'cg>(cg: &'cg CodeGen, fun: &BuiltIn, args: &[Expr], input_values: &[I64Ref<'cg>]) -> Result<CGValueRef<'cg>, CodeGenError> {
    let first_variable = &args[0];

    let mut cur = match first_variable {
        Expr::Variable(n) => {
            input_values[*n].clone().into()
        },
        Expr::Constant(n) => {
            generate_atom(cg, n)
        },
        Expr::Application(fun2, args2) => {
            generate_code_application(cg,&fun2, &args2, input_values)?
        },
    };

    for arg in args.iter().skip(1) {
        let next: CGValueRef<'cg> = match arg {
            Expr::Variable(n) => {
                input_values[*n].clone().into()
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
                generate_code_application(cg, &fun2, &folded_args, input_values)?
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

fn generate_code_inner<'cg>(cg: &'cg CodeGen, expr: &Expr, input_values: &[I64Ref<'cg>]) -> Result<CGValueRef<'cg>, CodeGenError> {
    Ok(match expr {
        Expr::Constant(e) => {
            return Err(CodeGenError::Const(e.clone()));
        },
        Expr::Variable(n) => {
            input_values[*n].clone().into()
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
                        input_values[*n].clone().into()
                    },
                    _ => {
                        generate_code_application(cg, fun, &folded_args, input_values)?
                    },
                }
            } else {
                generate_code_application(cg, fun, &folded_args, input_values)?
            }
        },
    })
}

pub fn generate_code(expr: &Expr, columns: usize, result_consumer: CodegenCFunctionSignature) -> Result<GeneratedCode, CodeGenError> {

    // TODO: I64 doesn't make sense for data length. Use U64 as soon as the wrapper is implemented
    let cg = CodeGen::new(&[DataType::Ptr, DataType::I64]);

    // Uncomment this and comment the generate_code_inner to test ifs

    /*let val = cg.get_arg(0);

    let summand = cg.get_arg(0) + 1;

    cg.gen_while(|| {
        val.clone().cg_lt(10)
    }, || {
        val.set(val.clone() + &summand);
    });
    let return_value = val.into();
    */

   
    /*
    let b1: BoolRef<'_> = cg.new_bool_const(false);
    cg.gen_if(b1, || {
        val.set(val.clone() + 1);
    });
    let return_value = val.into();
    */

    /*let b2 = (cg.get_arg(0) % 2).cg_eq(0);

    let val = cg.get_arg(0);

    cg.gen_if_else(b2, || {
        val.set(val.clone() + 1);
    }, || {
        val.set(val.clone() + 2);
    });
    let return_value = val.into();
    */

    let data_ptr = TypedPtrRef::<I64Ref>::from(cg.get_arg(0));
    let i = cg.new_i64_var(0);

   cg.gen_while::<CodeGenError>(|| {
        let num = I64Ref::from(cg.get_arg(1));
        Ok(i.clone().cg_lt(&num))
    }, || {
        // We assume we actually need the majority of our columns. We could also analyze the expression
        // And only load the columns that are actually used here.
        let row_ptr = data_ptr.typed_offset(&i);
        let row = (0..columns).map(|j| {
            row_ptr.clone().typed_offset(j as i64).read()
        }).collect::<Vec<_>>();
        let return_value: CGValueRef = generate_code_inner(&cg, expr, &row)?;
        cg.call_c_function(result_consumer, UntypedPtrRef::from(return_value));
        i.set(i.clone() + 1);
        Ok(())
    })?;

    cg.gen_return(None);

    let gc = cg.generate_code();

    Ok(gc)
}
