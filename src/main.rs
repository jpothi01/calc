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
    message: String,
}

impl fmt::Display for ParseError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}\n {}", &self.message, &self.context)
    }
}

impl fmt::Display for Op {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Op::Plus => "+",
            Op::Minus => "-",
            Op::Times => "*",
            Op::DividedBy => "/",
        };
        write!(f, "{}", &s)
    }
}

impl fmt::Display for Atom {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} {}", self.number, self.suffix)
    }
}

impl fmt::Display for Expr {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Expr::BinOp { op, lhs, rhs } => format!("({} {} {})", op, *lhs, *rhs),
            Expr::Negate { e } => format!("(- {})", *e),
            Expr::Atom(a) => format!("({})", a),
        };
        write!(f, "{}", &s)
    }
}

fn precedence(op: &Op) -> i32 {
    match op {
        Op::Plus => 0,
        Op::Minus => 0,
        Op::Times => 1,
        Op::DividedBy => 1,
    }
}

fn parse_number(s: &str) -> f64 {
    return s.parse().unwrap();
}

fn parse_atom(parser_state: &mut ParserState) -> Result<Atom, ParseError> {
    println!("parse_atom: {:?}", parser_state);

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

#[derive(Debug)]
struct ParserState<'a> {
    character_index: usize,
    remaining_input: &'a str,
}

impl<'a> ParserState<'a> {
    fn next_character(&self) -> Option<char> {
        self.remaining_input.chars().nth(0)
    }
    fn consume_character(&mut self) {
        self.remaining_input = &self.remaining_input[1..]
    }
}

fn string_to_op(c: char) -> Option<Op> {
    match c {
        '+' => Some(Op::Plus),
        '-' => Some(Op::Minus),
        '*' => Some(Op::Times),
        '/' => Some(Op::DividedBy),
        _ => None,
    }
}

fn parse_binop_rhs(
    parser_state: &mut ParserState,
    lhs: Expr,
    minimum_precedence: i32,
) -> Result<Expr, ParseError> {
    assert!(parser_state.remaining_input.len() > 0);
    println!("parse_binop_rhs: {:?}", parser_state);

    let mut new_lhs = lhs;
    loop {
        let maybe_next_character = parser_state.next_character();
        if maybe_next_character.is_none() {
            return Ok(new_lhs);
        }

        let next_character = maybe_next_character.unwrap();
        let maybe_op = string_to_op(next_character);
        if maybe_op.is_none() {
            if next_character != ')' {
                return Err(make_parse_error_with_msg(
                    parser_state,
                    "Expected operator.",
                ));
            }

            return Ok(new_lhs);
        }

        let op = maybe_op.unwrap();
        let op_precedence = precedence(&op);

        if op_precedence < minimum_precedence {
            // Our current expression has higher precedence than the next, so we're done collecting
            // the terms for it now.
            return Ok(new_lhs);
        }

        parser_state.consume_character();

        let next_primary_expr = parse_primary(parser_state)?;
        println!("next_primary_expr: {}", next_primary_expr);
        println!("{:?}", parser_state);

        // Clean this up
        let rhs = if parser_state.remaining_input.len() > 0
            && is_binop(parser_state.next_character().unwrap())
        {
            let next_op = string_to_op(parser_state.next_character().unwrap()).unwrap();
            let next_precedence = precedence(&next_op);
            if next_precedence > op_precedence {
                parse_binop_rhs(parser_state, next_primary_expr, op_precedence + 1)?
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
        println!("new_lhs: {}", new_lhs);
    }
}

fn make_parse_error_with_msg(parser_state: &ParserState, msg: &str) -> ParseError {
    ParseError {
        character_index: parser_state.character_index,
        context: String::from(parser_state.remaining_input),
        message: String::from(msg),
    }
}

fn make_parse_error(parser_state: &ParserState) -> ParseError {
    make_parse_error_with_msg(parser_state, "Error parsing input.")
}

fn parse_paren_expr(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    assert!(parser_state.next_character() == Some('('));
    parser_state.consume_character();
    let expr = parse_expr(parser_state)?;
    if parser_state.next_character() != Some(')') {
        return Err(make_parse_error(parser_state));
    }

    parser_state.consume_character();
    Ok(expr)
}

fn parse_primary(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    // Base case: We parse the rhs of a binary or unary operation
    // Recursive case: We need to parse the rhs of a binary operation
    println!("parse_primary: {:?}", parser_state);

    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_none() {
        return Err(make_parse_error(parser_state));
    }
    let next_character = maybe_next_character.unwrap();

    if next_character == '(' {
        return parse_paren_expr(parser_state);
    }

    return Ok(Expr::Atom(parse_atom(parser_state)?));
}

fn parse_expr(parser_state: &mut ParserState) -> Result<Expr, ParseError> {
    println!("parse_expr: {:?}", parser_state);
    let primary = parse_primary(parser_state)?;

    let maybe_next_character = parser_state.next_character();
    if maybe_next_character.is_some() {
        let next_character = maybe_next_character.unwrap();
        if is_binop(next_character) {
            parse_binop_rhs(parser_state, primary, -1)
        } else {
            Err(make_parse_error(parser_state))
        }
    } else {
        Ok(primary)
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
        remaining_input: &e_string,
        character_index: 0,
    };
    let expr_result = parse_expr(&mut parser_state);
    match expr_result {
        Err(e) => println!("{:?}", e),
        Ok(expr) => {
            println!("{}", &expr);
            println!("\n{}", eval(&expr))
        }
    }
}
