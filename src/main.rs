// Remove whitespace
// Recursive descent to build the tree of atoms
// Expand atoms
// Evaluate
use std::env;

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
    Atom {
        a: Box<Atom>,
    },
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
            print_expr(&**lhs);
            print_op(op);
            print_expr(&**rhs);
        }
        Expr::Atom { a } => print_atom(a),
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

fn parse_atom(s: &str) -> (Option<&str>, Option<Box<Atom>>) {
    let chars: Vec<char> = s.chars().collect();

    // For now only base 10 is supported
    if !chars[0].is_digit(10) {
        return (Some(s), None);
    }

    let first_non_numeric_index = chars.iter().position(|&c| !c.is_digit(10));
    if let None = first_non_numeric_index {
        return (
            None,
            Some(Box::new(Atom {
                number: parse_number(s),
                suffix: String::from(""),
            })),
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
        Some(&s[first_non_numeric_index.unwrap()..]),
        Some(Box::new(Atom {
            number: parse_number(numeric_part),
            suffix: String::from(suffix_part),
        })),
    );
}

fn parse(s: &str) -> Expr {
    // let mut p = -1;

    // for t in s.chars() {

    // }

    let (_, a) = parse_atom(s);
    Expr::Atom { a: a.unwrap() }
}

fn main() {
    let e_string = env::args()
        .skip(1)
        .fold(String::from(""), |s, arg| String::from(s) + &arg)
        .replace(" ", "");

    let e = parse(&e_string);
    print_expr(&e)
}
