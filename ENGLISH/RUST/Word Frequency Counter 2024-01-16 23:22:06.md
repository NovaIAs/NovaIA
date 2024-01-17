```rust
// Import necessary crates
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};

// Define the main function
fn main() {
    // Create a HashMap to store the words and their frequencies
    let mut word_counts: HashMap<String, usize> = HashMap::new();

    // Create a HashSet to store the unique words
    let mut unique_words: HashSet<String> = HashSet::new();

    // Read the input from the standard input
    let stdin = io::stdin();
    let mut lines = stdin.lock().lines();

    // Loop through the lines of input
    while let Some(line) = lines.next() {
        // Split the line into words
        let words: Vec<String> = line.unwrap().split_whitespace().map(|word| word.to_string()).collect();

        // Add the words to the HashMap and HashSet
        for word in words {
            // Convert the word to lowercase
            let word = word.to_lowercase();

            // Increment the word's frequency in the HashMap
            *word_counts.entry(word).or_insert(0) += 1;

            // Add the word to the HashSet
            unique_words.insert(word);
        }
    }

    // Print the unique words and their frequencies
    for (word, count) in word_counts {
        println!("{}: {}", word, count);
    }

    // Print the number of unique words
    println!("Total unique words: {}", unique_words.len());
}
```

This code is a Rust program that reads a text from the standard input, counts the occurrences of each word in the text, and prints the unique words and their frequencies.

The program uses a HashMap to store the words and their frequencies. A HashMap is a data structure that maps keys to values. In this case, the keys are the words and the values are the frequencies of the words.

The program also uses a HashSet to store the unique words. A HashSet is a data structure that stores a collection of unique elements. In this case, the elements are the unique words in the text.

The program reads the input from the standard input using the `io::stdin()` function. The `lines()` method on the `stdin` object returns an iterator over the lines of input.

The program loops through the lines of input and splits each line into words using the `split_whitespace()` method. The `split_whitespace()` method splits the line into words based on whitespace characters.

The program then loops through the words in the line and adds them to the HashMap and HashSet.

To add a word to the HashMap, the program uses the `entry()` method. The `entry()` method takes a key as an argument and returns a mutable reference to the value associated with that key. If the key does not exist in the HashMap, the `entry()` method creates a new entry for the key and returns a mutable reference to the new value.

To add a word to the HashSet, the program uses the `insert()` method. The `insert()` method takes a value as an argument and adds it to the HashSet. If the value is already in the HashSet, the `insert()` method does nothing.

After the program has processed all of the lines of input, it prints the unique words and their frequencies using the `println!()` macro. The `println!()` macro prints a formatted string to the standard output.

Finally, the program prints the number of unique words in the text using the `len()` method on the HashSet. The `len()` method returns the number of elements in the HashSet.