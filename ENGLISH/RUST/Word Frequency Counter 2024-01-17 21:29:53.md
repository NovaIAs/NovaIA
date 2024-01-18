// Import the necessary libraries.
use std::io::{self, BufRead};
use std::collections::HashMap;

// Define the main function.
fn main() {
    // Create a new HashMap to store the words and their counts.
    let mut word_counts: HashMap<String, usize> = HashMap::new();

    // Read the input from the user.
    let input = io::stdin().lock();

    // Loop over the lines of input.
    for line in input.lines() {
        // Get the line of input as a string.
        let line = line.unwrap();

        // Split the line into words.
        let words: Vec<&str> = line.split(' ').collect();

        // Loop over the words.
        for word in words {
            // Get the word as a string.
            let word = word.to_string();

            // Increment the count of the word in the HashMap.
            *word_counts.entry(word).or_insert(0) += 1;
        }
    }

    // Print the words and their counts.
    for (word, count) in word_counts {
        println!("{}: {}", word, count);
    }
}

// Explain the code
// The first line of the code imports the necessary libraries.
// The std::io::{self, BufRead} line imports the standard input/output library and the BufRead trait, which allows us to read lines of input from a buffer.
// The std::collections::HashMap line imports the HashMap data structure, which allows us to store the words and their counts.
// The second line of the code defines the main function, which is the entry point for the program.
// The third line of the code creates a new HashMap to store the words and their counts.
// The fourth line of the code reads the input from the user.
// The fifth line of the code loops over the lines of input.
// The sixth line of the code gets the line of input as a string.
// The seventh line of the code splits the line into words.
// The eighth line of the code loops over the words.
// The ninth line of the code gets the word as a string.
// The tenth line of the code increments the count of the word in the HashMap.
// The eleventh line of the code prints the words and their counts.
// The twelfth line of the code loops over the words and their counts.
// The thirteenth line of the code prints the word and its count.