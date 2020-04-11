// Remove whitespace
// Recursive descent to build the tree of atoms
// Expand atoms
// Evaluate
use std::env;
use std::error::Error;
use std::fmt;
use std::ops::Deref;

enum Op {
    Plus,
    Minus,
    Times,
    DividedBy,
}

struct Atom {
    number: f64,
    suffix: String,
}

enum Expr {
    BinOp {
        op: Op,
        lhs: Box<Expr>,
        rhs: Box<Expr>,
    },
    Negate {
        e: Box<Expr>,
    },
    Atom(Atom),
}

#[derive(Debug)]
struct ParseError {
    character_index: usize,
    context: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Error parsing expression: {}", &self.context)
    }
}

fn print_op(op: &Op) {
    match op {
        Plus => print!("+"),
        Minus => print!("-"),
        Times => print!("*"),
        DividedBy => print!("/"),
    }
}

fn print_atom(a: &Atom) {
    print!("({} {})", a.number, a.suffix)
}

fn print_expr(e: &Expr) {
    match e {
        Expr::BinOp { op, lhs, rhs } => {
            print_expr(lhs.deref());
            print_op(op);
            print_expr(rhs.deref());
        }
        Expr::Atom(a) => print_atom(a),
        _ => println!(""),
    }
}

fn precedence(op: Op) -> i32 {
    match op {
        Plus => 0,
        Minus => 0,
        Times => 1,
        Dividedby => 1,
    }
}

fn parse_number(s: &str) -> f64 {
    return s.parse().unwrap();
}

fn parse_atom(s: &str) -> (&str, Option<Atom>) {
    let chars: Vec<char> = s.chars().collect();

    // TODO: negative number support
    if s.len() == 0 {
        return (s, None);
    }

    // For now only base 10 is supported
    if !chars[0].is_digit(10) {
        return (s, None);
    }

    let first_non_numeric_index = chars.iter().position(|&c| !c.is_digit(10));
    if let None = first_non_numeric_index {
        return (
            &"",
            Some(Atom {
                number: parse_number(s),
                suffix: String::from(""),
            }),
        );
    }

    let numeric_part = &s[..first_non_numeric_index.unwrap()];

    let first_non_alphanumeric_index = chars.iter().position(|&c| !c.is_alphanumeric());
    let suffix_part = if let None = first_non_alphanumeric_index {
        &s[first_non_numeric_index.unwrap()..]
    } else {
        &s[first_non_numeric_index.unwrap()..first_non_alphanumeric_index.unwrap()]
    };

    // TODO suffix
    return (
        &s[first_non_numeric_index.unwrap()..],
        Some(Atom {
            number: parse_number(numeric_part),
            suffix: String::from(suffix_part),
        }),
    );
}

fn parse_binop(s: &str) -> (&str, Option<Op>) {
    match s.chars().nth(0) {
        Some('+') => (&s[1..], Some(Op::Plus)),
        Some('-') => (&s[1..], Some(Op::Minus)),
        Some('*') => (&s[1..], Some(Op::Times)),
        Some('/') => (&s[1..], Some(Op::DividedBy)),
        _ => (s, None),
    }
}

fn parse(user_input: &str) -> Result<Expr, ParseError> {
    let mut p = -1;

    let mut s: &str = user_input;
    let mut expr: Option<Expr> = None;

    while s.len() > 0 {
        let (next_s, maybe_atom) = parse_atom(s);
        s = next_s;
        match maybe_atom {
            None => panic!("Expected atom"),
            Some(atom) => {
                // See if this is part of a binary expression
                let (next_s, maybe_binop) = parse_binop(next_s);
                s = next_s;

                if let Some(binop) = maybe_binop {
                    let (next_s, maybe_atom) = parse_atom(s);
                    s = next_s;
                    expr = Some(Expr::BinOp {
                        op: binop,
                        lhs: Box::new(Expr::Atom(atom)),
                        rhs: Box::new(Expr::Atom(maybe_atom.unwrap())),
                    })
                }
            }
        }
    }

    match expr {
        None => Err(ParseError {
            character_index: 0,
            context: String::from(user_input),
        }),
        Some(e) => Ok(e),
    }
}

fn main() {
    let e_string = env::args()
        .skip(1)
        .fold(String::from(""), |s, arg| String::from(s) + &arg)
        .replace(" ", "");

    let expr_result = parse(&e_string);
    match expr_result {
        Err(e) => println!("{:?}", e),
        Ok(expr) => print_expr(&expr),
    }
}
