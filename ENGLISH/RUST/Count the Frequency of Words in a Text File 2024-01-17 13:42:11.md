```rust
// Import the necessary libraries.
use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};
use std::path::Path;

// Define the main function.
fn main() {
    // Create a HashMap to store the word frequencies.
    let mut word_frequencies: HashMap<String, u32> = HashMap::new();

    // Open the file containing the text.
    let file = File::open(Path::new("text.txt")).unwrap();

    // Create a BufReader to read the file line by line.
    let reader = BufReader::new(file);

    // Iterate over the lines of the file.
    for line in reader.lines() {
        // Get the line of text.
        let line = line.unwrap();

        // Split the line into words.
        let words = line.split_whitespace();

        // Iterate over the words in the line.
        for word in words {
            // Convert the word to lowercase.
            let word = word.to_lowercase();

            // Increment the frequency of the word in the HashMap.
            *word_frequencies.entry(word).or_insert(0) += 1;
        }
    }

    // Sort the word frequencies in descending order.
    let sorted_word_frequencies: Vec<(&String, &u32)> = word_frequencies.iter().collect();
    sorted_word_frequencies.sort_by(|a, b| b.1.cmp(a.1));

    // Print the top 10 most frequent words.
    println!("The top 10 most frequent words are:");
    for (word, frequency) in &sorted_word_frequencies[..10] {
        println!("{}: {}", word, frequency);
    }
}
```

This code reads a text file, splits the text into words, and counts the frequency of each word. The code then sorts the word frequencies in descending order and prints the top 10 most frequent words.

Here is a breakdown of the code:

* The `HashMap` library is used to store the word frequencies. A `HashMap` is a data structure that maps keys to values. In this case, the keys are the words and the values are the frequencies of the words.
* The `File` library is used to open the text file.
* The `BufReader` library is used to read the file line by line.
* The `lines()` method of the `BufReader` is used to iterate over the lines of the file.
* The `split_whitespace()` method of the `str` type is used to split the line of text into words.
* The `to_lowercase()` method of the `str` type is used to convert the word to lowercase.
* The `entry()` method of the `HashMap` is used to get the value associated with a key. If the key does not exist, the `entry()` method inserts a new key-value pair into the `HashMap`.
* The `or_insert()` method of the `Option` type is used to get the value associated with a key. If the key does not exist, the `or_insert()` method returns a default value.
* The `sort_by()` method of the `Vec` type is used to sort the word frequencies in descending order.
* The `[..10]` slice is used to get the top 10 most frequent words.
* The `println!()` macro is used to print the top 10 most frequent words.