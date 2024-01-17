```rust
// This program generates a random number between 1 and 100, then asks the user to guess the number.
// If the user guesses correctly, the program prints a message saying so.
// If the user guesses incorrectly, the program prints a message saying so and gives the user a hint.

use rand::Rng;
use std::io;

fn main() {
    // Generate a random number between 1 and 100.
    let secret_number = rand::thread_rng().gen_range(1..=100);

    // Create a loop that will continue until the user guesses the secret number.
    loop {
        // Prompt the user to guess the number.
        println!("Guess a number between 1 and 100:");

        // Read the user's input.
        let mut guess = String::new();
        io::stdin().read_line(&mut guess).expect("Failed to read line");

        // Convert the user's input to a number.
        let guess: u32 = match guess.trim().parse() {
            Ok(num) => num,
            Err(_) => {
                println!("Invalid input. Please enter a number between 1 and 100.");
                continue;
            }
        };

        // Check if the user guessed the secret number.
        if guess == secret_number {
            println!("Congratulations! You guessed the number.");
            break;
        } else if guess < secret_number {
            println!("Your guess is too low. Try again.");
        } else {
            println!("Your guess is too high. Try again.");
        }

        // Give the user a hint.
        println!("Hint: The secret number is between {} and {}.",
                 guess.min(secret_number),
                 guess.max(secret_number));
    }
}
```

Explanation:

* The `rand` crate is used to generate a random number.
* The `std::io` crate is used to read input from the user.
* The `loop` keyword is used to create a loop that will continue until the user guesses the secret number.
* The `println!` macro is used to print messages to the user.
* The `read_line` method is used to read a line of input from the user.
* The `trim` method is used to remove any leading or trailing whitespace from the user's input.
* The `parse` method is used to convert the user's input to a number.
* The `match` expression is used to handle the result of the `parse` method.
* The `min` and `max` methods are used to find the minimum and maximum values of two numbers.