```rust
// This program implements a basic calculator that can perform addition, subtraction,
// multiplication, and division. It uses the `match` expression to handle different
// operations.

use std::io;

fn main() {
    // Get the input from the user.
    println!("Enter the first number:");
    let mut first_number = String::new();
    io::stdin().read_line(&mut first_number).expect("Failed to read input");
    let first_number: f64 = first_number.trim().parse().expect("Failed to parse input");

    println!("Enter the second number:");
    let mut second_number = String::new();
    io::stdin().read_line(&mut second_number).expect("Failed to read input");
    let second_number: f64 = second_number.trim().parse().expect("Failed to parse input");

    // Get the operation from the user.
    println!("Enter the operation (+, -, *, /):");
    let mut operation = String::new();
    io::stdin().read_line(&mut operation).expect("Failed to read input");
    let operation = operation.trim();

    // Calculate the result.
    let result = match operation {
        "+" => first_number + second_number,
        "-" => first_number - second_number,
        "*" => first_number * second_number,
        "/" => first_number / second_number,
        _ => panic!("Invalid operation"),
    };

    // Print the result.
    println!("The result is {}", result);
}
```

This code is a basic calculator that can perform addition, subtraction, multiplication, and division. It uses the `match` expression to handle different operations.

The first step is to get the input from the user. The `println!` macro is used to print a message to the user, and the `io::stdin().read_line()` function is used to read a line of input from the user.

The next step is to parse the input into a floating-point number. The `trim()` method is used to remove any whitespace from the input, and the `parse()` method is used to convert the input into a floating-point number.

The next step is to get the operation from the user. The `println!` macro is used to print a message to the user, and the `io::stdin().read_line()` function is used to read a line of input from the user.

The next step is to calculate the result. The `match` expression is used to handle different operations. The `+`, `-`, `*`, and `/` operators are used to perform addition, subtraction, multiplication, and division, respectively. If the user enters an invalid operation, the `panic!` macro is used to print an error message and terminate the program.

The last step is to print the result. The `println!` macro is used to print a message to the user, and the `{}` placeholder is used to insert the result into the message.