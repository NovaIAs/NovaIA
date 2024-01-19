```rust
// Import the necessary libraries.
use std::io;
use std::collections::HashMap;

// Define the main function.
fn main() {
    // Create a hash map to store the words and their counts.
    let mut word_counts: HashMap<String, usize> = HashMap::new();

    // Create a loop to read the input from the user.
    loop {
        // Prompt the user to enter a sentence.
        println!("Enter a sentence:");

        // Read the input from the user.
        let mut input = String::new();
        io::stdin().read_line(&mut input).expect("Failed to read input");

        // Remove the newline character from the input.
        input = input.trim_end().to_string();

        // Check if the user entered "exit" to exit the program.
        if input == "exit" {
            break;
        }

        // Split the input into words.
        let words: Vec<&str> = input.split(' ').collect();

        // Loop through the words and update the word counts.
        for word in words {
            // Convert the word to lowercase.
            let word = word.to_lowercase();

            // Get the current count of the word.
            let count = word_counts.get(&word).unwrap_or(&0);

            // Increment the count of the word.
            word_counts.insert(word, count + 1);
        }
    }

    // Print the word counts.
    for (word, count) in word_counts {
        println!("{}: {}", word, count);
    }
}
```

This code is a program that counts the number of times each word appears in a series of sentences entered by the user. The program uses a hash map to store the words and their counts. The program first prompts the user to enter a sentence. It then reads the input from the user and removes the newline character. The program then checks if the user entered "exit" to exit the program. If the user did not enter "exit", the program splits the input into words. The program then loops through the words and updates the word counts. The program finally prints the word counts.