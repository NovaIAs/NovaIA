```rust

use std::collections::HashMap;
use std::fs::File;
use std::io::{BufRead, BufReader};

fn main() {
    let file = File::open("input.txt").unwrap();
    let reader = BufReader::new(file);

    let mut frequencies: HashMap<String, i32> = HashMap::new();

    for line in reader.lines() {
        let line = line.unwrap();
        let words: Vec<&str> = line.split_whitespace().collect();

        for word in words {
            let word = word.to_lowercase();
            let count = frequencies.entry(word).or_insert(0);
            *count += 1;
        }
    }

    let mut sorted_frequencies: Vec<(&String, &i32)> = frequencies.iter().collect();
    sorted_frequencies.sort_by(|a, b| b.1.cmp(a.1));

    for (word, count) in sorted_frequencies {
        println!("{}: {}", word, count);
    }
}

```

This code reads a text file, tokenizes it into words, and counts the frequency of each word. It then prints the words and their frequencies in descending order.

Here's a breakdown of the code:

- The `HashMap` data structure is used to store the word frequencies. The key is the word, and the value is the frequency.
- The `BufReader` type is used to read the lines of the text file one by one.
- The `split_whitespace()` method is used to split each line of the text file into words.
- The `to_lowercase()` method is used to convert each word to lowercase.
- The `entry()` method is used to get a mutable reference to the value associated with a key in the `HashMap`. If the key does not exist, it is created with a value of 0.
- The `*` operator is used to dereference the value associated with a key in the `HashMap`.
- The `sort_by()` method is used to sort the `HashMap` entries by the frequency of each word.
- The `println!()` macro is used to print the words and their frequencies to the console.