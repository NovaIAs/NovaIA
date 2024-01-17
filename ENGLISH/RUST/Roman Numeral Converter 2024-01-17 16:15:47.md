```rust
// Import necessary libraries.
use std::io;
use std::fmt;
use std::collections::HashMap;

// Define the main function.
fn main() {
    // Create a HashMap to store the Roman numerals and their corresponding integer values.
    let roman_numerals: HashMap<&str, u32> = HashMap::from([
        ("I", 1),
        ("V", 5),
        ("X", 10),
        ("L", 50),
        ("C", 100),
        ("D", 500),
        ("M", 1000),
    ]);

    // Create a loop to continuously prompt the user for Roman numerals.
    loop {
        // Prompt the user to enter a Roman numeral.
        println!("Enter a Roman numeral (or press Enter to exit): ");

        // Read the user's input.
        let input = read_line();

        // Check if the user wants to exit.
        if input.is_empty() {
            break;
        }

        // Convert the Roman numeral to an integer.
        let integer_value = convert_roman_numeral(&roman_numerals, &input);

        // Print the result.
        println!("The integer value of {} is {}.", input, integer_value);
    }
}

// Define a function to read a line of input from the user.
fn read_line() -> String {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Could not read input.");
    input.trim().to_string()
}

// Define a function to convert a Roman numeral to an integer.
fn convert_roman_numeral(roman_numerals: &HashMap<&str, u32>, numeral: &str) -> u32 {
    // Create a variable to store the integer value of the Roman numeral.
    let mut integer_value = 0;

    // Iterate over the characters of the Roman numeral.
    for (i, character) in numeral.chars().enumerate() {
        // Get the integer value of the current character.
        let current_value = roman_numerals.get(&character.to_string()).unwrap();

        // If the current character is the last character, add its value to the integer value.
        if i == numeral.len() - 1 {
            integer_value += current_value;
        } else {
            // If the current character is smaller than the next character, subtract its value from the integer value.
            let next_value = roman_numerals.get(&numeral.chars().nth(i + 1).unwrap().to_string()).unwrap();
            if current_value < next_value {
                integer_value -= current_value;
            } else {
                // Otherwise, add its value to the integer value.
                integer_value += current_value;
            }
        }
    }

    // Return the integer value of the Roman numeral.
    integer_value
}
```

This code is a program that converts Roman numerals to their corresponding integer values. It uses a loop to continuously prompt the user for Roman numerals and then calls the `convert_roman_numeral()` function to convert the Roman numeral to an integer. The `convert_roman_numeral()` function takes two arguments: a HashMap containing the Roman numerals and their corresponding integer values, and the Roman numeral to be converted. The function iterates over the characters of the Roman numeral and adds their values to the integer value, taking into account the special rules of Roman numeral subtraction. The final integer value is returned by the function.