```rust
// This is a Rust program that implements a simple calculator.

// Import the standard input/output library.
use std::io;

// Define the main function.
fn main() {
    // Create a loop that will run until the user quits.
    loop {
        // Prompt the user to enter an expression.
        println!("Enter an expression (or 'quit' to exit):");

        // Read the user's input.
        let input = io::stdin().read_line().unwrap();

        // Check if the user entered 'quit'.
        if input.trim() == "quit" {
            break;
        }

        // Parse the user's input into a mathematical expression.
        let expr = match expr::parse(&input) {
            Ok(expr) => expr,
            Err(err) => {
                println!("Error: {}", err);
                continue;
            }
        };

        // Evaluate the mathematical expression.
        let result = match expr.eval() {
            Ok(result) => result,
            Err(err) => {
                println!("Error: {}", err);
                continue;
            }
        };

        // Print the result of the evaluation.
        println!("Result: {}", result);
    }
}

// Define the `Expr` enum, which represents a mathematical expression.
enum Expr {
    // A number.
    Number(f64),

    // A binary operator.
    BinaryOp {
        op: char,
        left: Box<Expr>,
        right: Box<Expr>,
    },
}

// Implement the `parse` function, which parses a string into a mathematical expression.
impl Expr {
    fn parse(input: &str) -> Result<Expr, String> {
        // Create a parser object.
        let mut parser = Parser::new(input);

        // Parse the expression.
        let expr = parser.parse_expr()?;

        // Check if there are any unparsed characters left.
        if !parser.is_empty() {
            return Err("Unparsed characters at the end of the input".to_string());
        }

        // Return the expression.
        Ok(expr)
    }
}

// Define the `eval` function, which evaluates a mathematical expression.
impl Expr {
    fn eval(&self) -> Result<f64, String> {
        // Match the expression on its type.
        match self {
            // If the expression is a number, return the number.
            Expr::Number(n) => Ok(*n),

            // If the expression is a binary operator, evaluate the left and right subexpressions
            // and then apply the operator.
            Expr::BinaryOp { op, left, right } => {
                let left = left.eval()?;
                let right = right.eval()?;

                // Apply the operator.
                match op {
                    '+' => Ok(left + right),
                    '-' => Ok(left - right),
                    '*' => Ok(left * right),
                    '/' => {
                        if right == 0.0 {
                            Err("Division by zero".to_string())
                        } else {
                            Ok(left / right)
                        }
                    },
                    _ => Err(format!("Invalid operator: {}", op)),
                }
            }
        }
    }
}

// Define the `Parser` struct, which represents a parser for mathematical expressions.
struct Parser<'a> {
    // The input string.
    input: &'a str,

    // The current position in the input string.
    pos: usize,
}

// Implement the `new` function, which creates a new `Parser` object.
impl<'a> Parser<'a> {
    fn new(input: &'a str) -> Parser<'a> {
        Parser {
            input,
            pos: 0,
        }
    }
}

// Implement the `parse_expr` function, which parses a mathematical expression.
impl<'a> Parser<'a> {
    fn parse_expr(&mut self) -> Result<Expr, String> {
        // Parse the left subexpression.
        let left = self.