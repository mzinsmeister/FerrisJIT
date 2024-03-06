//! This is a parser example taken from the nom create!

//! In this example we build an [S-expression](https://en.wikipedia.org/wiki/S-expression)
//! parser and tiny [lisp](https://en.wikipedia.org/wiki/Lisp_(programming_language)) interpreter.
//! Lisp is a simple type of language made up of Atoms and Lists, forming easily parsable trees.

use std::fmt::{self, Display, Formatter};

use nom::{
  branch::alt,
  bytes::complete::{tag, tag_no_case},
  character::complete::{char, digit1, multispace0, multispace1},
  combinator::{cut, map, map_res, opt},
  error::{context, VerboseError},
  multi::many0,
  sequence::{delimited, preceded, terminated, tuple},
  IResult, Parser,
};

use crate::{codegen::ir::DataType, query_codegen::get_type};

/// We start by defining the types that define the shape of data that we want.
/// In this case, we want something tree-like

/// Starting from the most basic, we define some built-in functions that our lisp has
#[derive(Debug, PartialEq, Clone, Copy)]
pub enum BuiltIn {
  Plus,
  Minus,
  Times,
  Divide,
  Equal,
  NotEqual,
  LessThan,
  GreaterThan,
  LessThanOrEqual,
  GreaterThanOrEqual,
  And,
  Or,
  /*Not,*/
}

/// We now wrap this type and a few other primitives into our Atom type.
/// Remember from before that Atoms form one half of our language.

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum Atom {
  Num(i64),
  Boolean(bool),
}

impl Atom {
  pub fn get_num(&self) -> i64 {
    match self {
      Atom::Num(n) => *n,
      _ => panic!("Not a number"),
    }
  }

  pub fn get_bool(&self) -> bool {
    match self {
      Atom::Boolean(b) => *b,
      _ => panic!("Not a boolean"),
    }
  }
}

impl Display for Atom {
  fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
    match self {
      Atom::Num(n) => write!(f, "{}", n),
      Atom::Boolean(b) => write!(f, "{}", b),
    }
  }
}

/// The remaining half is Lists. We implement these as recursive Expressions.
/// For a list of numbers, we have `'(1 2 3)`, which we'll parse to:
/// ```
/// Expr::Quote(vec![Expr::Constant(Atom::Num(1)),
///                  Expr::Constant(Atom::Num(2)),
///                  Expr::Constant(Atom::Num(3))])
/// Quote takes an S-expression and prevents evaluation of it, making it a data
/// structure that we can deal with programmatically. Thus any valid expression
/// is also a valid data structure in Lisp itself.

#[derive(Debug, PartialEq, Clone)]
pub enum Expr {
  Constant(Atom),
  Variable(usize),
  /// (func-name arg1 arg2)
  Application(BuiltIn, Vec<Expr>),
  /*/// (if predicate do-this)
  If(Box<Expr>, Box<Expr>),
  /// (if predicate do-this otherwise-do-this)
  IfElse(Box<Expr>, Box<Expr>, Box<Expr>),
  /// '(3 (if (+ 3 3) 4 5) 7)
  Quote(Vec<Expr>),*/
}


#[derive(Debug, PartialEq, Eq, Copy, Clone)]
pub enum AggregateFunc {
  Sum,
  Prod,
  Avg,
  Max,
  Min
  // TODO: Add more aggregate functions here
}

#[derive(Debug, PartialEq, Clone)]
pub struct Query {
  pub aggregate: Option<AggregateFunc>,
  pub filter: Option<Expr>, // Must be a boolean expression
  pub expr: Expr // Must be an integer expression
}

/// Continuing the trend of starting from the simplest piece and building up,
/// we start by creating a parser for the built-in operator functions.
fn parse_builtin_op<'a>(i: &'a str) -> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
  let (i, t) = terminated(alt((
    tag("+"),
    tag("-"),
    tag("*"),
    tag("/"),
    tag("!="),
    tag("<"),
    tag(">"),
    tag("<="),
    tag(">="),
    tag("&"),
    tag("|"),
    //tag("not"),
  )), multispace1)(i)?;

  // because we are matching single character tokens, we can do the matching logic
  // on the returned value
  Ok((
    i,
    match t {
      "+" => BuiltIn::Plus,
      "-" => BuiltIn::Minus,
      "*" => BuiltIn::Times,
      "/" => BuiltIn::Divide,
      "=" => BuiltIn::Equal,
      "!=" => BuiltIn::NotEqual,
      "<" => BuiltIn::LessThan,
      ">" => BuiltIn::GreaterThan,
      "<=" => BuiltIn::LessThanOrEqual,
      ">=" => BuiltIn::GreaterThanOrEqual,
      "&" => BuiltIn::And,
      "|" => BuiltIn::Or,
      //"not" => BuiltIn::Not,
      _ => unreachable!(),
    },
  ))
}

fn parse_builtin<'a>(i: &'a str) -> IResult<&'a str, BuiltIn, VerboseError<&'a str>> {
  // alt gives us the result of first parser that succeeds, of the series of
  // parsers we give it
  alt((
    parse_builtin_op,
    // map lets us process the parsed output, in this case we know what we parsed,
    // so we ignore the input and return the BuiltIn directly
    //map(tag("not"), |_| BuiltIn::Not),
  ))(i)
}

/// Our boolean values are also constant, so we can do it the same way
fn parse_bool<'a>(i: &'a str) -> IResult<&'a str, Atom, VerboseError<&'a str>> {
  alt((
    map(tag("#t"), |_| Atom::Boolean(true)),
    map(tag("#f"), |_| Atom::Boolean(false)),
  ))(i)
}

/// The next easiest thing to parse are keywords.
/// We introduce some error handling combinators: `context` for human readable errors
/// and `cut` to prevent back-tracking.
///
/// Put plainly: `preceded(tag(":"), cut(alpha1))` means that once we see the `:`
/// character, we have to see one or more alphabetic chararcters or the input is invalid.
/*fn parse_keyword<'a>(i: &'a str) -> IResult<&'a str, Atom, VerboseError<&'a str>> {
  map(
    context("keyword", preceded(tag(":"), cut(alpha1))),
    |sym_str: &str| Atom::Keyword(sym_str.to_string()),
  )(i)
}*/

/// Next up is number parsing. We're keeping it simple here by accepting any number (> 1)
/// of digits but ending the program if it doesn't fit into an i32.
fn parse_num(i: &str) -> IResult<&str, Atom, VerboseError<&str>> {
  alt((
    map_res(digit1, |digit_str: &str| {
      digit_str.parse::<i64>().map(Atom::Num)
    }),
    map(preceded(tag("-"), digit1), |digit_str: &str| {
      Atom::Num(-digit_str.parse::<i64>().unwrap())
    }),
  ))
  .parse(i)
}

/// Now we take all these simple parsers and connect them.
/// We can now parse half of our language!
fn parse_atom<'a>(i: &'a str) -> IResult<&'a str, Atom, VerboseError<&'a str>> {
  alt((
    parse_num,
    parse_bool,
    //map(parse_builtin, Atom::BuiltIn),
    //parse_keyword,
  ))(i)
}

/// We then add the Expr layer on top
fn parse_constant<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  map(parse_atom, |atom| Expr::Constant(atom))(i)
}

/// We add variables in the format $n where n is a number
fn parse_variable<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  map_res(preceded(tag("$"), digit1), |digit_str: &str| {
    digit_str.parse::<usize>().map(Expr::Variable)
  })(i)
}

/// Before continuing, we need a helper function to parse lists.
/// A list starts with `(` and ends with a matching `)`.
/// By putting whitespace and newline parsing here, we can avoid having to worry about it
/// in much of the rest of the parser.
///
/// Unlike the previous functions, this function doesn't take or consume input, instead it
/// takes a parsing function and returns a new parsing function.
fn s_exp<'a, O1, F>(inner: F) -> impl FnMut(&'a str) -> IResult<&'a str, O1, VerboseError<&'a str>>
where
  F: Parser<&'a str, O1, VerboseError<&'a str>>,
{
  delimited(
    char('('),
    preceded(multispace0, inner),
    context("closing paren", cut(preceded(multispace0, char(')')))),
  )
}

/// We can now use our new combinator to define the rest of the `Expr`s.
///
/// Starting with function application, we can see how the parser mirrors our data
/// definitions: our definition is `Application(Box<Expr>, Vec<Expr>)`, so we know
/// that we need to parse an expression and then parse 0 or more expressions, all
/// wrapped in an S-expression.
///
/// `tuple` is used to sequence parsers together, so we can translate this directly
/// and then map over it to transform the output into an `Expr::Application`
fn parse_application<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  let application_inner = map(tuple((parse_builtin, many0(parse_expr))), |(head, tail)| {
    Expr::Application(head, tail)
  });
  // finally, we wrap it in an s-expression
  s_exp(application_inner)(i)
}

/// Because `Expr::If` and `Expr::IfElse` are so similar (we easily could have
/// defined `Expr::If` to have an `Option` for the else block), we parse both
/// in a single function.
///
/// In fact, we define our parser as if `Expr::If` was defined with an Option in it,
/// we have the `opt` combinator which fits very nicely here.
/*fn parse_if<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  let if_inner = context(
    "if expression",
    map(
      preceded(
        // here to avoid ambiguity with other names starting with `if`, if we added
        // variables to our language, we say that if must be terminated by at least
        // one whitespace character
        terminated(tag("if"), multispace1),
        cut(tuple((parse_expr, parse_expr, opt(parse_expr)))),
      ),
      |(pred, true_branch, maybe_false_branch)| {
        if let Some(false_branch) = maybe_false_branch {
          Expr::IfElse(
            Box::new(pred),
            Box::new(true_branch),
            Box::new(false_branch),
          )
        } else {
          Expr::If(Box::new(pred), Box::new(true_branch))
        }
      },
    ),
  );
  s_exp(if_inner)(i)
}*/

/// A quoted S-expression is list data structure.
///
/// This example doesn't have the symbol atom, but by adding variables and changing
/// the definition of quote to not always be around an S-expression, we'd get them
/// naturally.
/*fn parse_quote<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  // this should look very straight-forward after all we've done:
  // we find the `'` (quote) character, use cut to say that we're unambiguously
  // looking for an s-expression of 0 or more expressions, and then parse them
  map(
    context("quote", preceded(tag("'"), cut(s_exp(many0(parse_expr))))),
    |exprs| Expr::Quote(exprs),
  )(i)
}*/

/// We tie them all together again, making a top-level expression parser!

fn parse_expr<'a>(i: &'a str) -> IResult<&'a str, Expr, VerboseError<&'a str>> {
  preceded(
    multispace0,
    alt((parse_constant, parse_variable, parse_application/*, parse_if, parse_quote*/)),
  )(i)
}


/// And that's it!
/// We can now parse our entire lisp language.
///
/// But in order to make it a little more interesting, we can hack together
/// a little interpreter to take an Expr, which is really an
/// [Abstract Syntax Tree](https://en.wikipedia.org/wiki/Abstract_syntax_tree) (AST),
/// and give us something back

/// To start we define a couple of helper functions
#[allow(dead_code)]
fn get_num_from_expr(e: Expr, vars: &[&i64]) -> Option<i64> {
  match e {
    Expr::Constant(Atom::Num(n)) => Some(n),
    Expr::Variable(i) => Some(*vars[i]),
    _ => None
  }
}

/*fn get_bool_from_expr(e: Expr) -> Option<bool> {
  if let Expr::Constant(Atom::Boolean(b)) = e {
    Some(b)
  } else {
    None
  }
}*/

/// This function tries to reduce the AST.
/// This has to return an Expression rather than an Atom because quoted s_expressions
/// can't be reduced
pub fn eval_expression(e: &Expr, vars: &[i64]) -> Option<Atom> {
  match e {
    // Constants and quoted s-expressions are our base-case
    Expr::Constant(c) /*| Expr::Quote(_)*/ => Some(c.clone()),
    Expr::Variable(i) => Some(Atom::Num(vars[*i])),
    // we then recursively `eval_expression` in the context of our special forms
    // and built-in operators
    /*Expr::If(pred, true_branch) => {
      let reduce_pred = eval_expression(*pred)?;
      if get_bool_from_expr(reduce_pred)? {
        eval_expression(*true_branch)
      } else {
        None
      }
    }
    Expr::IfElse(pred, true_branch, false_branch) => {
      let reduce_pred = eval_expression(*pred)?;
      if get_bool_from_expr(reduce_pred)? {
        eval_expression(*true_branch)
      } else {
        eval_expression(*false_branch)
      }
    }*/
    Expr::Application(op, tail) => {
      let reduced_tail = tail
        .into_iter()
        .map(|expr| eval_expression(expr, vars))
        .collect::<Option<Vec<Atom>>>()?;
      match op {
        BuiltIn::Plus | BuiltIn::Times | BuiltIn::Divide | BuiltIn::Minus 
        | BuiltIn::LessThan | BuiltIn::GreaterThan | BuiltIn::LessThanOrEqual 
        | BuiltIn::GreaterThanOrEqual => {
          // Check that all the tail expressions are numbers
          let nums = reduced_tail.iter().map(|a| if let Atom::Num(n) = a { Some(*n) } else { return None }).collect::<Option<Vec<i64>>>()?;
          match op {
            BuiltIn::Plus => Some(Atom::Num(nums.iter().sum())),
            BuiltIn::Times => Some(Atom::Num(nums.iter().product())),
            BuiltIn::Minus => Some(Atom::Num(nums.iter().skip(1).fold(nums[0], |a, f| a - f))),
            BuiltIn::Divide => Some(Atom::Num(nums.iter().skip(1).fold(nums[0], |a, f| a / f))),
            BuiltIn::LessThan => Some(Atom::Boolean(nums.iter().skip(1).all(|&x| nums[0] < x))),
            BuiltIn::GreaterThan => Some(Atom::Boolean(nums.iter().skip(1).all(|&x| nums[0] > x))),
            BuiltIn::LessThanOrEqual => Some(Atom::Boolean(nums.iter().skip(1).all(|&x| nums[0] <= x))),
            BuiltIn::GreaterThanOrEqual => Some(Atom::Boolean(nums.iter().skip(1).all(|&x| nums[0] >= x))),
            _ => unreachable!(),
          }
        },
        BuiltIn::Equal => Some(Atom::Boolean(
          reduced_tail
            .iter()
            .zip(reduced_tail.iter().skip(1))
            .all(|(a, b)| a == b),
        )),
        BuiltIn::NotEqual => Some(Atom::Boolean(
          reduced_tail
            .iter()
            .zip(reduced_tail.iter().skip(1))
            .any(|(a, b)| a != b),
        )),
        // Bitwise operations on integers and normal and/or with boolean result on booleans
        BuiltIn::And => {
          if let Atom::Boolean(_) = &reduced_tail[0] {
            Some(Atom::Boolean(reduced_tail.iter().all(|a| a.get_bool())))
          } else {
            Some(Atom::Num(reduced_tail.iter().fold(-1, |a, b| a & b.get_num())))
          }
        },
        BuiltIn::Or => {
          if let Atom::Boolean(_) = &reduced_tail[0] {
            Some(Atom::Boolean(reduced_tail.iter().any(|a| a.get_bool())))
          } else {
            Some(Atom::Num(reduced_tail.iter().fold(0, |a, b| a | b.get_num())))
          }
        },
        /*BuiltIn::Not => {
          if reduced_tail.len() != 1 {
            return None;
          } else {
            Atom::Boolean(!get_bool_from_expr(reduced_tail.first().cloned().unwrap())?)
          }
        },*/
      }
    }
  }
}

pub fn run_query(query: &Query, data: &[i64], columns: usize, mut result_consumer: impl FnMut(Atom)) {
  let expr = &query.expr;
  let filter = &query.filter;
  let aggregate = query.aggregate;

  let mut aggregate_value = 0;

  for row in data.chunks_exact(columns) {
    if let Some(filter) = filter {
      if let Atom::Boolean(false) = eval_expression(filter, row).unwrap() {
        continue;
      }
    }
    let value = if let Atom::Num(n) = eval_expression(expr, row).unwrap() {
      n
    } else {
      panic!("Main expression must produce an integer");
    };
    match aggregate {
      Some(AggregateFunc::Sum) => aggregate_value += value,
      None => result_consumer(Atom::Num(value)),
      _ => panic!("Unsupported aggregate function"),
    }
  }
  if let Some(_) = aggregate {
    result_consumer(Atom::Num(aggregate_value));
  }
}

fn err_converter(e: nom::Err<VerboseError<&str>>) -> String {
  match e {
    nom::Err::Error(e) | nom::Err::Failure(e) => nom::error::convert_error("", e),
    nom::Err::Incomplete(_) => "Incomplete input".to_string(),
  }
}

#[allow(dead_code)]
pub fn parse_expr_from_str(src: &str) -> Result<Expr, String> {
  parse_expr(src)
    .map_err( err_converter)
    .map(|(_, exp)| exp)
}
/// And we add one more top-level function to tie everything together, letting
/// us call eval on a string directly
#[allow(dead_code)]
pub fn eval_from_str(src: &str, vars: &[i64]) -> Result<Atom, String> {
  parse_expr(src)
    .map_err(err_converter)
    .and_then(|(_, exp)| eval_expression(&exp, vars).ok_or("Eval Error".to_string()))
}

fn parse_aggregate_func<'a>(i: &'a str) -> IResult<&'a str, AggregateFunc, VerboseError<&'a str>> {
  alt((
    map(tag_no_case("sum"), |_| AggregateFunc::Sum),
    map(tag_no_case("prod"), |_| AggregateFunc::Prod),
    map(tag_no_case("avg"), |_| AggregateFunc::Avg),
    map(tag_no_case("max"), |_| AggregateFunc::Max),
    map(tag_no_case("min"), |_| AggregateFunc::Min),
  ))(i)
}

pub fn parse_query_from_str(src: &str) -> Result<Query, String> {
  let (src, aggregate) = opt(terminated(parse_aggregate_func, multispace1))(src).map_err(err_converter)?;
  let (src, expr) = parse_expr(src).unwrap();
  if get_type(&expr) != DataType::I64 {
    return Err("Expression must be an integer expression".to_string());
  }
  let (_, filter) = opt(preceded(tuple((multispace1, tag_no_case("where"), multispace1)), parse_expr))(src).map_err(err_converter)?;
  if let Some(filter) = &filter {
    if get_type(filter) != DataType::Bool {
      return Err("Filter must be a boolean expression".to_string());
    }
  }
  Ok(Query { aggregate, filter, expr })
}