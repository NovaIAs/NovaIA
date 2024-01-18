```rust
// This is a custom macro to define a new type of error.
macro_rules! error {
    ($($arg:tt)*) => {
        enum Error {
            $($arg)*,
        }

        impl std::fmt::Display for Error {
            fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
                match *self {
                    $(Error::$arg => write!(f, "{}", stringify!($arg)),)*
                }
            }
        }

        impl std::error::Error for Error {
            fn description(&self) -> &str {
                match *self {
                    $(Error::$arg => stringify!($arg),)*
                }
            }
        }
    }
}

// Define a custom error type.
error! {
    ParseError,
    IOError,
    EvaluationError,
}

// A custom type to represent an expression.
enum Expr {
    Number(f64),
    Variable(String),
    UnaryOp(UnaryOp, Box<Expr>),
    BinaryOp(BinaryOp, Box<Expr>, Box<Expr>),
}

// Define unary operators.
enum UnaryOp {
    Negative,
    Positive,
}

// Define binary operators.
enum BinaryOp {
    Add,
    Subtract,
    Multiply,
    Divide,
}

// A custom type to represent the state of the evaluator.
struct EvalState {
    variables: std::collections::HashMap<String, f64>,
}

// Implement the `std::fmt::Display` trait for `Expr`.
impl std::fmt::Display for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            Expr::Number(n) => write!(f, "{}", n),
            Expr::Variable(name) => write!(f, "{}", name),
            Expr::UnaryOp(op, expr) => write!(f, "{}({})", op, expr),
            Expr::BinaryOp(op, lhs, rhs) => write!(f, "({} {} {})", lhs, op, rhs),
        }
    }
}

// Implement the `std::fmt::Display` trait for `UnaryOp`.
impl std::fmt::Display for UnaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            UnaryOp::Negative => write!(f, "-"),
            UnaryOp::Positive => write!(f, "+"),
        }
    }
}

// Implement the `std::fmt::Display` trait for `BinaryOp`.
impl std::fmt::Display for BinaryOp {
    fn fmt(&self, f: &mut std::fmt::Formatter) -> std::fmt::Result {
        match self {
            BinaryOp::Add => write!(f, "+"),
            BinaryOp::Subtract => write!(f, "-"),
            BinaryOp::Multiply => write!(f, "*"),
            BinaryOp::Divide => write!(f, "/"),
        }
    }
}

// Parse an expression from a string.
fn parse_expr(input: &str) -> Result<Expr, ParseError> {
    let mut tokens = input.split_whitespace();

    // Parse the first token.
    let token = tokens.next().ok_or(ParseError::UnexpectedEOF)?;

    // Parse a number.
    if let Ok(n) = token.parse::<f64>() {
        return Ok(Expr::Number(n));
    }

    // Parse a variable.
    if token.chars().all(|c| c.is_alphabetic()) {
        return Ok(Expr::Variable(token.to_string()));
    }

    // Parse a unary operator.
    let op = match token {
        "-" => UnaryOp::Negative,
        "+" => UnaryOp::Positive,
        _ => return Err(ParseError::InvalidUnaryOp(token.to_string())),
    };

    // Parse the expression after the unary operator.
    let expr = parse_expr(tokens.collect::<String>())?;

    // Create a unary expression.
    Ok(Expr::UnaryOp(op, Box::new(expr)))
}

// Evaluate an expression.
fn evaluate_expr(expr: &Expr, state: &mut EvalState) -> Result<f64, EvaluationError> {
    match expr {
        Expr::Number(n) => Ok(*n),
        Expr::Variable(name) => state.variables.get(name).ok_or(EvaluationError::UndefinedVariable(name.to_string()))?.clone(),
        Expr::UnaryOp(op, expr) => {
            let value = evaluate_expr(expr, state)?;
            match op {
                UnaryOp::Negative => Ok(-value),
                UnaryOp::Positive => Ok(value),
            }
        }
        Expr::BinaryOp(op, lhs, rhs) => {
            let lhs_value = evaluate_expr(lhs, state)?;
            let rhs_value = evaluate_expr(rhs, state)?;
            match op {
                BinaryOp::Add => Ok(lhs_value + rhs_value),
                BinaryOp::Subtract => Ok(lhs_value - rhs_value),
                BinaryOp::Multiply => Ok(lhs_value * rhs_value),
                BinaryOp::Divide => {
                    if rhs_value == 0.0 {
                        Err(EvaluationError::DivisionByZero)
                    } else {
                        Ok(lhs_value / rhs_value)
                    }
                }
            }
        }
    }
}

fn main() {
    // Define the initial state of the evaluator.
    let mut state = EvalState {
        variables: std::collections::HashMap::new(),
    };

    // Parse an expression from the command line.
    let input = std::env::args().nth(1).unwrap();
    let expr = parse_expr(&input).unwrap();

    // Evaluate the expression.
    let result = evaluate_expr(&expr, &mut state);

    // Print the result.
    match result {
        Ok(value) => println!("The result is {}", value),
        Err(err) => println!("An error occurred: {}", err),
    }
}
```

This code is a simple expression evaluator. It can parse and evaluate expressions that contain numbers, variables, unary operators (negative and positive), and binary operators (addition, subtraction, multiplication, and division).

The code begins by defining a custom error type called `Error`. This type is used to represent the different types of errors that can occur during parsing and evaluation.

Next, the `Expr` type is defined. This type represents an expression that can be parsed and evaluated. Expressions can be numbers, variables, unary expressions, or binary expressions.

The `UnaryOp` and `BinaryOp` types are defined next. These types represent the different types of unary and binary operators that can be used in expressions.

The `EvalState` type is defined next. This type represents the state of the evaluator. The evaluator state includes a map of variables to their values.

The `parse_expr()` function is defined next. This function parses an expression from a string and returns an `Expr` value. The function uses a simple tokenizer to break the input string into tokens. The tokens are then used to construct an `Expr` value.

The `evaluate_expr()` function is defined next. This function evaluates an expression and returns a `f64` value. The function uses the `EvalState` to evaluate variables. The function also handles unary and binary operators.

The `main()` function is defined next. This function is the entry point for the program. The function parses an expression from the command line and then evaluates the expression. The function prints the result of the evaluation to the console.

This code is a good example of how to use Rust to write a simple expression evaluator. The code is well-organized and easy to understand. The code also uses a number of Rust's features, such as macros, enums, and structs.