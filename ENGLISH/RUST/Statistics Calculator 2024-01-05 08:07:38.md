```rust
use std::{
    fs::File,
    io::{prelude::*, BufReader},
    path::Path,
};

// Function to read a file and return its contents as a string
fn read_file(file_path: &Path) -> Result<String, std::io::Error> {
    // Open the file in read-only mode
    let file = File::open(file_path)?;

    // Create a buffered reader to read the file line by line
    let reader = BufReader::new(file);

    // Read the file line by line and append each line to a string
    let mut contents = String::new();
    for line in reader.lines() {
        contents.push_str(&line?);
        contents.push('\n');
    }

    // Return the contents of the file as a string
    Ok(contents)
}

// Function to parse a string of numbers and return a vector of integers
fn parse_numbers(input: &str) -> Result<Vec<i32>, std::num::ParseIntError> {
    // Split the input string into a vector of strings, using the comma as a delimiter
    let numbers = input.split(',').collect::<Vec<_>>();

    // Parse each string in the vector into an integer
    let mut parsed_numbers = Vec::new();
    for number in numbers {
        parsed_numbers.push(number.parse::<i32>()?);
    }

    // Return the vector of integers
    Ok(parsed_numbers)
}

// Function to calculate the sum of a vector of integers
fn sum_numbers(numbers: &[i32]) -> i32 {
    // Initialize the sum to 0
    let mut sum = 0;

    // Add each number in the vector to the sum
    for number in numbers {
        sum += number;
    }

    // Return the sum
    sum
}

// Function to find the median of a vector of integers
fn median(numbers: &[i32]) -> f64 {
    // Sort the vector in ascending order
    let mut sorted_numbers = numbers.to_vec();
    sorted_numbers.sort();

    // Calculate the length of the sorted vector
    let length = sorted_numbers.len();

    // If the length is even, the median is the average of the two middle numbers
    if length % 2 == 0 {
        let mid1 = sorted_numbers[length / 2 - 1];
        let mid2 = sorted_numbers[length / 2];
        return (mid1 + mid2) as f64 / 2.0;
    }
    // If the length is odd, the median is the middle number
    else {
        return sorted_numbers[length / 2] as f64;
    }
}

// Function to find the mode of a vector of integers
fn mode(numbers: &[i32]) -> i32 {
    // Create a hash map to store the frequency of each number
    let mut frequency_map = std::collections::HashMap::new();

    // Iterate over the vector and update the frequency of each number
    for number in numbers {
        *frequency_map.entry(number).or_insert(0) += 1;
    }

    // Find the number with the highest frequency
    let mut max_frequency = 0;
    let mut mode = 0;
    for (number, frequency) in frequency_map.iter() {
        if frequency > &max_frequency {
            max_frequency = *frequency;
            mode = *number;
        }
    }

    // Return the mode
    mode
}

// Main function
fn main() {
    // Read the input file
    let file_path = Path::new("input.txt");
    let input = read_file(file_path).unwrap();

    // Parse the input string into a vector of integers
    let numbers = parse_numbers(&input).unwrap();

    // Calculate the sum of the numbers
    let sum = sum_numbers(&numbers);

    // Calculate the median of the numbers
    let median = median(&numbers);

    // Calculate the mode of the numbers
    let mode = mode(&numbers);

    // Print the results to the console
    println!("Sum: {}", sum);
    println!("Median: {}", median);
    println!("Mode: {}", mode);
}
```

Explanation:

This Rust program reads a text file containing a list of comma-separated integers, parses the integers from the file, and calculates the sum, median, and mode of the numbers.

Here's a breakdown of the code:

1. **Reading the File:**
   - The `read_file` function is used to read the contents of the input file and return the contents as a string.

2. **Parsing the Numbers:**
   - The `parse_numbers` function is used to parse the string of numbers into a vector of integers. It uses the `split` method to split the string into a vector of strings, and then the `parse::<i32>()` method to parse each string into an integer.

3. **Calculating the Sum:**
   - The `sum_numbers` function is used to calculate the sum of the numbers in the vector. It simply iterates over the vector and adds each number to a running total.

4. **Calculating the Median:**
   - The `median` function is used to calculate the median of the numbers in the vector. It first sorts the vector in ascending order, and then calculates the median depending on whether the length of the vector is even or odd.

5. **Calculating the Mode:**
   - The `mode` function is used to calculate the mode of the numbers in the vector. It uses a hash map to store the frequency of each number, and then finds the number with the highest frequency.

6. **Main Function:**
   - The `main` function is the entry point of the program. It calls the `read_file` function to read the input file, the `parse_numbers` function to parse the numbers, and the `sum_numbers`, `median`, and `mode` functions to calculate the sum, median, and mode of the numbers. It then prints the results to the console.