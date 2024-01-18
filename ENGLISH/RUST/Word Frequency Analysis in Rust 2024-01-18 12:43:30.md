```rust
// Import the necessary libraries.
use std::io;
use std::collections::HashMap;

// Define a struct to represent a word and its frequency.
struct WordFrequency {
    word: String,
    frequency: u32,
}

// Define a function to read a line of text from the standard input.
fn read_line() -> String {
    let mut line = String::new();
    io::stdin().read_line(&mut line).unwrap();
    line.trim().to_string()
}

// Define a function to tokenize a line of text into words.
fn tokenize(line: &str) -> Vec<String> {
    let mut words = Vec::new();
    for word in line.split_whitespace() {
        words.push(word.to_string());
    }
    words
}

// Define a function to count the frequency of each word in a list of words.
fn count_frequencies(words: &[String]) -> HashMap<String, u32> {
    let mut frequencies: HashMap<String, u32> = HashMap::new();
    for word in words {
        *frequencies.entry(word.clone()).or_insert(0) += 1;
    }
    frequencies
}

// Define a function to print the word frequencies in descending order of frequency.
fn print_frequencies(frequencies: &HashMap<String, u32>) {
    let mut frequencies: Vec<(String, u32)> = frequencies.iter().collect();
    frequencies.sort_by(|a, b| b.1.cmp(&a.1));
    for (word, frequency) in frequencies {
        println!("{}: {}", word, frequency);
    }
}

// Define a function to find the most frequent word in a list of words.
fn find_most_frequent_word(words: &[String]) -> Option<&String> {
    let frequencies = count_frequencies(words);
    let most_frequent_word = frequencies.iter().max_by(|a, b| a.1.cmp(&b.1));
    most_frequent_word.map(|word| &word.0)
}

// Define a function to find the least frequent word in a list of words.
fn find_least_frequent_word(words: &[String]) -> Option<&String> {
    let frequencies = count_frequencies(words);
    let least_frequent_word = frequencies.iter().min_by(|a, b| a.1.cmp(&b.1));
    least_frequent_word.map(|word| &word.0)
}

// Define a function to calculate the average frequency of words in a list of words.
fn calculate_average_frequency(words: &[String]) -> f32 {
    let frequencies = count_frequencies(words);
    let total_frequency: u32 = frequencies.values().sum();
    let average_frequency = total_frequency as f32 / words.len() as f32;
    average_frequency
}

// Define a function to calculate the median frequency of words in a list of words.
fn calculate_median_frequency(words: &[String]) -> f32 {
    let frequencies = count_frequencies(words);
    let mut frequencies: Vec<u32> = frequencies.values().copied().collect();
    frequencies.sort();
    let median_frequency = if frequencies.len() % 2 == 0 {
        (frequencies[frequencies.len() / 2 - 1] + frequencies[frequencies.len() / 2]) as f32 / 2.0
    } else {
        frequencies[frequencies.len() / 2] as f32
    };
    median_frequency
}

// Define a function to calculate the mode frequency of words in a list of words.
fn calculate_mode_frequency(words: &[String]) -> f32 {
    let frequencies = count_frequencies(words);
    let mut max_frequency = 0;
    let mut mode_frequency = 0;
    for frequency in frequencies.values() {
        if *frequency > max_frequency {
            max_frequency = *frequency;
            mode_frequency = *frequency;
        } else if *frequency == max_frequency {
            mode_frequency = max_frequency;
        }
    }
    mode_frequency as f32
}

// Get the input from the user.
println!("Enter a line of text:");
let line = read_line();

// Tokenize the line of text into words.
let words = tokenize(&line);

// Count the frequency of each word in the line of text.
let frequencies = count_frequencies(&words);

// Print the word frequencies in descending order of frequency.
println!("Word frequencies:");
print_frequencies(&frequencies);

// Find the most frequent word in the line of text.
let most_frequent_word =