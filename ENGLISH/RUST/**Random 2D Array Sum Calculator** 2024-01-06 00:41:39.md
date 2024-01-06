```rust
// This Rust code generates a random 2D array of integers, calculates the sum of each row and column, and prints the results.

// Import the necessary libraries.
use rand::prelude::*;
use std::fmt;

// Define a struct to represent a 2D array.
#[derive(Debug)]
struct Array2D {
    data: Vec<Vec<i32>>,
    rows: usize,
    cols: usize,
}

// Implement the Display trait for the Array2D struct.
impl fmt::Display for Array2D {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        for row in &self.data {
            for col in row {
                write!(f, "{:3}", col)?;
            }
            writeln!(f)?;
        }
        Ok(())
    }
}

// Generate a random 2D array of integers.
fn generate_array(rows: usize, cols: usize) -> Array2D {
    let mut rng = rand::thread_rng();
    let data: Vec<Vec<i32>> = (0..rows)
        .map(|_| (0..cols).map(|_| rng.gen_range(0, 100)).collect())
        .collect();
    Array2D { data, rows, cols }
}

// Calculate the sum of each row and column in the array.
fn calculate_sums(array: &Array2D) -> (Vec<i32>, Vec<i32>) {
    let mut row_sums: Vec<i32> = Vec::with_capacity(array.rows);
    let mut col_sums: Vec<i32> = Vec::with_capacity(array.cols);

    for row in &array.data {
        let row_sum: i32 = row.iter().sum();
        row_sums.push(row_sum);
    }

    for col in 0..array.cols {
        let col_sum: i32 = array.data.iter().map(|row| row[col]).sum();
        col_sums.push(col_sum);
    }

    (row_sums, col_sums)
}

// Print the array and the sums of each row and column.
fn print_results(array: &Array2D, row_sums: &Vec<i32>, col_sums: &Vec<i32>) {
    println!("Array:");
    println!("{}", array);

    println!("Row Sums:");
    for row_sum in row_sums {
        println!("{:3}", row_sum);
    }

    println!("Column Sums:");
    for col_sum in col_sums {
        println!("{:3}", col_sum);
    }
}

// Main function.
fn main() {
    // Generate a random 2D array.
    let array = generate_array(5, 5);

    // Calculate the sum of each row and column in the array.
    let (row_sums, col_sums) = calculate_sums(&array);

    // Print the array and the sums of each row and column.
    print_results(&array, &row_sums, &col_sums);
}
```

**Explanation:**

1. The code first imports the necessary libraries, including the `rand` library for generating random numbers and the `std::fmt` library for formatting output.
2. It defines a struct called `Array2D` to represent a 2D array. The struct has three fields: `data`, which stores the actual data in the array, `rows`, which stores the number of rows in the array, and `cols`, which stores the number of columns in the array.
3. The `impl fmt::Display for Array2D` block implements the `Display` trait for the `Array2D` struct, allowing it to be printed using the `println!` macro.
4. The `generate_array()` function generates a random 2D array of integers. It uses the `rand` library to generate random numbers and creates a vector of vectors to store the data.
5. The `calculate_sums()` function calculates the sum of each row and column in the array. It uses two vectors, `row_sums` and `col_sums`, to store the sums.
6. The `print_results()` function prints the array and the sums of each row and column. It uses the `Display` trait to print the array and the `println!` macro to print the sums.
7. The `main()` function calls the `generate_array()`, `calculate_sums()`, and `print_results()` functions to generate a random 2D array, calculate the sums of each row and column, and print the results.