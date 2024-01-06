```rust
// Import the necessary crates.
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

// Define a struct to represent a word and its frequency.
#[derive(Debug)]
struct WordFrequency {
    word: String,
    frequency: usize,
}

// Define a function to read a file and return a list of words.
fn read_file(file_path: &Path) -> Vec<String> {
    // Open the file in read-only mode.
    let file = File::open(file_path).unwrap();

    // Create a buffered reader to read the file line by line.
    let reader = BufReader::new(file);

    // Create a vector to store the words.
    let mut words = Vec::new();

    // Read each line of the file and split it into words.
    for line in reader.lines() {
        let line = line.unwrap();
        let words_in_line = line.split_whitespace();

        // Add the words to the vector.
        for word in words_in_line {
            words.push(word.to_string());
        }
    }

    // Return the vector of words.
    words
}

// Define a function to count the frequency of each word in a list of words.
fn count_word_frequency(words: Vec<String>) -> HashMap<String, usize> {
    // Create a hash map to store the word frequencies.
    let mut word_frequencies: HashMap<String, usize> = HashMap::new();

    // Iterate over the words in the list.
    for word in words {
        // Get the current frequency of the word.
        let frequency = word_frequencies.get(&word).unwrap_or(&0);

        // Increment the frequency of the word.
        word_frequencies.insert(word, frequency + 1);
    }

    // Return the hash map of word frequencies.
    word_frequencies
}

// Define a function to print the word frequencies in descending order.
fn print_word_frequencies(word_frequencies: HashMap<String, usize>) {
    // Create a vector of word frequencies.
    let mut word_frequencies: Vec<WordFrequency> = word_frequencies
        .iter()
        .map(|(word, frequency)| WordFrequency {
            word: word.clone(),
            frequency: *frequency,
        })
        .collect();

    // Sort the word frequencies in descending order by frequency.
    word_frequencies.sort_by(|a, b| b.frequency.cmp(&a.frequency));

    // Print the word frequencies.
    for word_frequency in word_frequencies {
        println!("{}: {}", word_frequency.word, word_frequency.frequency);
    }
}

// Define the main function.
fn main() {
    // Get the file path from the command line.
    let file_path = Path::new("/path/to/file.txt");

    // Read the file and get a list of words.
    let words = read_file(file_path);

    // Count the frequency of each word in the list of words.
    let word_frequencies = count_word_frequency(words);

    // Print the word frequencies in descending order.
    print_word_frequencies(word_frequencies);
}
```

This code reads a text file, counts the frequency of each word in the file, and then prints the word frequencies in descending order. The code is divided into several functions, each of which performs a specific task.

The `read_file()` function reads the text file and returns a list of words. The `count_word_frequency()` function takes a list of words and returns a hash map of word frequencies. The `print_word_frequencies()` function takes a hash map of word frequencies and prints the word frequencies in descending order.

The main function calls the `read_file()`, `count_word_frequency()`, and `print_word_frequencies()` functions to perform the task of reading the text file, counting the word frequencies, and printing the word frequencies in descending order.