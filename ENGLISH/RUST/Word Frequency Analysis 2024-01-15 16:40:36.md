```rust
// Import the necessary libraries.
use std::collections::{HashMap, HashSet};
use std::io::{self, BufRead};

// Define the main function.
fn main() {
    // Create a hash map to store the words and their frequencies.
    let mut word_freq = HashMap::new();

    // Create a hash set to store the unique words.
    let mut unique_words = HashSet::new();

    // Read the input from stdin.
    let stdin = io::stdin();

    // Loop over the lines of the input.
    for line in stdin.lock().lines() {
        // Unwrap the line, which may be an error.
        let line = line.unwrap();

        // Split the line into words.
        let words = line.split_whitespace();

        // Loop over the words.
        for word in words {
            // Convert the word to lowercase.
            let word = word.to_lowercase();

            // Add the word to the hash map and increment its frequency.
            let count = word_freq.entry(word).or_insert(0);
            *count += 1;

            // Add the word to the hash set.
            unique_words.insert(word);
        }
    }

    // Print the number of unique words.
    println!("Number of unique words: {}", unique_words.len());

    // Print the top 10 most frequent words.
    let mut top_10_words = word_freq.iter().collect::<Vec<_>>();
    top_10_words.sort_by(|a, b| b.1.cmp(&a.1));
    println!("Top 10 most frequent words:");
    for (word, freq) in &top_10_words[..10] {
        println!("{}: {}", word, freq);
    }
}
```

This code reads a text file from stdin, tokenizes it into words, and stores the words and their frequencies in a hash map. It also stores the unique words in a hash set. The code then prints the number of unique words and the top 10 most frequent words.

Here is a detailed explanation of the code:

* The `main` function is the entry point of the program.
* The `HashMap` type is a hash map, which is a data structure that maps keys to values. In this case, the keys are words and the values are their frequencies.
* The `HashSet` type is a hash set, which is a data structure that stores unique values. In this case, the values are unique words.
* The `io::stdin()` function returns a handle to the standard input stream.
* The `lock()` method on the stdin handle locks the stream, which is necessary to call the `lines()` method.
* The `lines()` method on the stdin handle returns an iterator over the lines of the input.
* The `unwrap()` method on the line iterator unwraps the line, which may be an error.
* The `split_whitespace()` method on the line splits the line into words.
* The `to_lowercase()` method on the word converts the word to lowercase.
* The `entry()` method on the hash map returns a mutable reference to the value associated with the key, or creates a new entry if the key does not exist.
* The `or_insert()` method on the entry returns the value associated with the key, or inserts the default value if the key does not exist.
* The `insert()` method on the hash set inserts the word into the set.
* The `len()` method on the hash set returns the number of unique words.
* The `iter()` method on the hash map returns an iterator over the key-value pairs in the map.
* The `collect()` method on the iterator collects the key-value pairs into a vector.
* The `sort_by()` method on the vector sorts the key-value pairs by the value, in descending order.
* The `println!()` macro prints the number of unique words and the top 10 most frequent words.