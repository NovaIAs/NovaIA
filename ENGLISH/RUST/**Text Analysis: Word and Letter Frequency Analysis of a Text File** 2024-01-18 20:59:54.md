```rust
use std::{
    collections::{BTreeMap, HashMap, HashSet},
    fs::File,
    io::{BufRead, BufReader},
    iter,
    slice::Iter,
};

fn main() {
    let file = File::open("input.txt").expect("Unable to open file");
    let reader = BufReader::new(file);

    // Read lines from the file into a vector of strings
    let lines: Vec<String> = reader
        .lines()
        .map(|line| line.expect("Unable to read line"))
        .collect();

    // Create a hash map to store the frequency of each word in the file
    let mut word_counts: HashMap<String, usize> = HashMap::new();

    // Iterate over each line in the file
    for line in lines {
        // Split the line into words using the whitespace character as a delimiter
        let words: Vec<String> = line.split_whitespace().map(|word| word.to_string()).collect();

        // Iterate over each word in the line
        for word in words {
            // Convert the word to lowercase and remove any punctuation marks
            let word = word.to_lowercase().replace(|c: char| !c.is_alphabetic(), "");

            // Increment the frequency of the word in the hash map
            *word_counts.entry(word).or_insert(0) += 1;
        }
    }

    // Convert the hash map into a sorted vector of tuples, where each tuple consists of a word and its frequency
    let sorted_word_counts: Vec<(String, usize)> = word_counts
        .iter()
        .map(|(word, count)| (word.clone(), *count))
        .collect();

    // Sort the vector of tuples by the frequency of the words in descending order
    let sorted_word_counts = sorted_word_counts
        .iter()
        .sorted_by(|a, b| b.1.cmp(&a.1))
        .collect();

    // Print the top 10 most frequently occurring words in the file
    println!("Top 10 Most Frequently Occurring Words:");
    for (word, count) in sorted_word_counts[..10].iter() {
        println!("{}: {}", word, count);
    }

    // Create a set to store the unique words in the file
    let unique_words: HashSet<String> = word_counts.keys().cloned().collect();

    // Print the number of unique words in the file
    println!("Number of Unique Words: {}", unique_words.len());

    // Calculate the average word length in the file
    let total_word_length: usize = sorted_word_counts
        .iter()
        .map(|(word, count)| word.len() * count)
        .sum();
    let average_word_length: f64 = total_word_length as f64 / word_counts.len() as f64;

    // Print the average word length
    println!("Average Word Length: {:.2}", average_word_length);

    // Find the longest word in the file
    let longest_word: Option<&String> = sorted_word_counts
        .iter()
        .max_by_key(|(_, count)| count)
        .map(|(word, _)| word);

    // Print the longest word
    println!("Longest Word: {}", longest_word.unwrap_or(&"".to_string()));

    // Find the shortest word in the file
    let shortest_word: Option<&String> = sorted_word_counts
        .iter()
        .min_by_key(|(_, count)| count)
        .map(|(word, _)| word);

    // Print the shortest word
    println!("Shortest Word: {}", shortest_word.unwrap_or(&"".to_string()));

    // Create a tree map to store the frequency of each letter in the file
    let mut letter_counts: BTreeMap<char, usize> = BTreeMap::new();

    // Iterate over each word in the file
    for word in &lines {
        // Iterate over each character in the word
        for character in word.chars() {
            // Increment the frequency of the character in the tree map
            *letter_counts.entry(character).or_insert(0) += 1;
        }
    }

    // Print the top 10 most frequently occurring letters in the file
    println!("Top 10 Most Frequently Occurring Letters:");
    for (letter, count) in letter_counts.iter().rev().take(10) {
        println!("{}: {}", letter, count);
    }
}
```

Explanation:

1. **Reading the Input File:**
   - We open the input file using the `File::open()` function and create a `BufReader` to read the file line by line.
   - The `lines()` method is used to create a vector of strings, where each string represents a line from the file.

2. **Counting Word Frequencies:**
   - We create a `HashMap` called `word_counts` to store the frequency of each word in the file.
   - We iterate over each line in the file and split it into words using the whitespace character as a delimiter.
   - We convert each word to lowercase, remove any punctuation marks, and increment its frequency in the `word_counts` hash map.

3. **Sorting Word Frequencies:**
   - We convert the `word_counts` hash map into a sorted vector of tuples, where each tuple consists of a word and its frequency.
   - We sort the vector of tuples by the frequency of the words in descending order.

4. **Printing Top 10 Most Frequently Occurring Words:**
   - We print the top 10 most frequently occurring words in the file along with their frequencies.

5. **Counting Unique Words:**
   - We create a `HashSet` called `unique_words` to store the unique words in the file.
   - We populate the `unique_words` set by iterating over the keys of the `word_counts` hash map.

6. **Calculating Average Word Length:**
   - We calculate the total word length by summing the product of each word's length and its frequency.
   - We divide the total word length by the number of words to get the average word length.

7. **Finding Longest and Shortest Words:**
   - We find the longest and shortest words in the file by iterating over the sorted list of words and selecting the word with the maximum and minimum length, respectively.

8. **Counting Letter Frequencies:**
   - We create a `BTreeMap` called `letter_counts` to store the frequency of each letter in the file.
   - We iterate over each word in the file and count the frequency of each letter in the word.

9. **Printing Top 10 Most Frequently Occurring Letters:**
   - We print the top 10 most frequently occurring letters in the file along with their frequencies.