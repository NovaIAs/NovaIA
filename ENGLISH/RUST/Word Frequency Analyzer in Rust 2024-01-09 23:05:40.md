```rust
// Import the necessary libraries.
use std::io;
use std::collections::HashMap;
use std::collections::BTreeMap;
use std::collections::HashSet;
use std::iter::FromIterator;

// Define the main function.
fn main() {
    // Create a hash map to store the word frequencies.
    let mut word_frequencies: HashMap<String, usize> = HashMap::new();

    // Read the input text from the user.
    let mut input = String::new();
    io::stdin().read_line(&mut input).expect("Could not read input");

    // Split the input text into words.
    let words: Vec<&str> = input.split_whitespace().collect();

    // Iterate over the words and update the word frequencies.
    for word in words {
        let word = word.to_lowercase();
        let count = word_frequencies.entry(word).or_insert(0);
        *count += 1;
    }

    // Create a BTreeMap to store the sorted word frequencies.
    let mut sorted_word_frequencies: BTreeMap<String, usize> = BTreeMap::new();

    // Iterate over the word frequencies and add them to the sorted BTreeMap.
    for (word, frequency) in word_frequencies {
        sorted_word_frequencies.insert(word, frequency);
    }

    // Create a HashSet to store the unique words.
    let unique_words: HashSet<String> = HashSet::from_iter(sorted_word_frequencies.keys().cloned());

    // Print the sorted word frequencies.
    for (word, frequency) in sorted_word_frequencies {
        println!("{}: {}", word, frequency);
    }

    // Print the number of unique words.
    println!("Number of unique words: {}", unique_words.len());
}
```

This code reads a text from the user, splits it into words, and counts the frequency of each word. It then sorts the word frequencies and prints them out, along with the number of unique words in the text.

The code uses several Rust features, including:

* Hash maps: A hash map is a data structure that stores key-value pairs. In this code, the hash map is used to store the word frequencies.
* BTreeMap: A BTreeMap is a sorted map data structure. In this code, the BTreeMap is used to store the sorted word frequencies.
* HashSet: A HashSet is a set data structure that stores unique elements. In this code, the HashSet is used to store the unique words in the text.
* Iterators: Iterators are objects that allow you to iterate over a collection of data. In this code, iterators are used to iterate over the words in the input text and over the word frequencies.
* Closures: Closures are anonymous functions that can be passed around and executed. In this code, closures are used to update the word frequencies and to sort the word frequencies.

This code is a good example of how to use Rust to solve a common problem. It demonstrates how to use Rust's data structures and iterators to efficiently process data.