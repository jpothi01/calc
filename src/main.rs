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
        Op::Plus => print!("+"),
        Op::Minus => print!("-"),
        Op::Times => print!("*"),
        Op::DividedBy => print!("/"),
    }
}

fn print_atom(a: &Atom) {
    print!("({} {})", a.number, a.suffix)
}

fn print_expr(e: &Expr) {
    match e {
        Expr::BinOp { op, lhs, rhs } => {
            print!("(binop ");
            print_expr(lhs.deref());
            print_op(op);
            print_expr(rhs.deref());
            print!(")");
        }
        Expr::Atom(a) => print_atom(a),
        _ => println!(""),
    }
}

fn precedence(op: &Op) -> i32 {
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

fn parse_atom(parser_state: &mut ParserState) -> Result<Atom, ParseError> {
    let s = parser_state.remaining_input;
    let chars: Vec<char> = s.chars().collect();

    assert!(s.len() > 0);
    // TODO: negative number support

    // For now only base 10 is supported
    if !chars[0].is_digit(10) {
        return Err(make_parse_error(parser_state));
    }

    let first_non_numeric_index = chars.iter().position(|&c| !c.is_digit(10));
    if let None = first_non_numeric_index {
        parser_state.remaining_input = "";
        return Ok(Atom {
            number: parse_number(s),
            suffix: String::from(""),
        });
    }

    let numeric_part = &s[..first_non_numeric_index.unwrap()];

    let first_non_alphanumeric_index = chars.iter().position(|&c| !c.is_alphanumeric());
    let (suffix_part, rest) = if let None = first_non_alphanumeric_index {
        (&s[first_non_numeric_index.unwrap()..], "")
    } else {
        (
            &s[first_non_numeric_index.unwrap()..first_non_alphanumeric_index.unwrap()],
            &s[first_non_alphanumeric_index.unwrap()..],
        )
    };

    parser_state.remaining_input = rest;
    return Ok(Atom {
        number: parse_number(numeric_part),
        suffix: String::from(suffix_part),
    });
}

fn is_binop(c: char) -> bool {
    "+-*/".contains(c)
}

struct ParserState<'a> {
    current_precedence: i32,
    character_index: usize,
    remaining_input: &'a str,
}

fn parse_binop_rhs(parser_state: &mut ParserState, lhs: Expr) -> Result<Expr, ParseError> {
    assert!(parser_state.remaining_input.len() > 0);

    let mut new_lhs = lhs;
    loop {
        let maybe_next_character = parser_state.remaining_input.chars().nth(0);
        if maybe_next_character.is_none() {
            return Ok(new_lhs);
        }

        let next_character = maybe_next_character.unwrap();
        let maybe_op = match next_character {
            '+' => Some(Op::Plus),
            '-' => Some(Op::Minus),
            '*' => Some(Op::Times),
            '/' => Some(Op::DividedBy),
            _ => None,
        };
        if maybe_op.is_none() {
            // No more operators to parse, we're done
            return Ok(new_lhs);
        }

        let op = maybe_op.unwrap();
        let op_precedence = precedence(&op);

        parser_state.remaining_input = &parser_state.remaining_input[1..];

        let next_primary_expr = parse_primary(parser_state)?;
        let rhs = if op_precedence > parser_state.current_precedence {
            if parser_state.remaining_input.len() > 0
                && is_binop(parser_state.remaining_input.chars().nth(0).unwrap())
            {
                // RHS will be the result of the rest of the parse
                parse_binop_rhs(parser_state, next_primary_expr)?
            } else {
                next_primary_expr
            }
        } else {
            next_primary_expr
        };

        new_lhs = Expr::BinOp {
            op: op,
            lhs: Box::new(new_lhs),
            rhs: Box::new(rhs),
        };
    }
}

fn make_parse_error(parser_state: &ParserState) -> ParseError {
    ParseError {
        character_index: parser_state.character_index,
        context: String::from(parser_state.remaining_input),
    }
}

fn parse_primary(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    // Base case: We parse the rhs of a binary or unary operation
    // Recursive case: We need to parse the rhs of a binary operation

    let atom = parse_atom(parser_state)?;

    let maybe_next_character = parser_state.remaining_input.chars().nth(0);
    match maybe_next_character {
        Some(next_character) => {
            // "(" => {
            //     // Parse sub-expression
            // }
            // ")" => {
            //     // Return current expression
            // }
            if is_binop(next_character) {
                parse_binop_rhs(parser_state, Expr::Atom(atom))
            } else {
                Err(make_parse_error(parser_state))
            }
        }
        None => Ok(Expr::Atom(atom)),
    }
}

fn eval(expr: &Expr) -> f64 {
    match expr {
        Expr::BinOp { op, lhs, rhs } => match op {
            Op::Plus => eval(lhs) + eval(rhs),
            Op::Minus => eval(lhs) - eval(rhs),
            Op::Times => eval(lhs) * eval(rhs),
            Op::DividedBy => eval(lhs) / eval(rhs),
        },
        Expr::Negate { e } => -eval(e),
        Expr::Atom(a) => match a.suffix.as_str() {
            "K" => a.number * 1024f64,
            "k" => a.number * 1000f64,
            "M" => a.number * 1024f64 * 1024f64,
            "m" => a.number * 1000f64 * 1000f64,
            _ => a.number,
        },
    }
}

fn main() {
    let e_string = env::args()
        .skip(1)
        .fold(String::from(""), |s, arg| String::from(s) + &arg)
        .replace(" ", "");

    let mut parser_state = ParserState {
        current_precedence: 0,
        remaining_input: &e_string,
        character_index: 0,
    };
    let expr_result = parse_primary(&mut parser_state);
    match expr_result {
        Err(e) => println!("{:?}", e),
        Ok(expr) => {
            print_expr(&expr);
            println!("\n{}", eval(&expr))
        }
    }
}
