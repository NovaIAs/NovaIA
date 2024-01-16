```rust
// Import the necessary libraries.
use std::{fs, io, path};
use std::io::Write;
use std::path::Path;
use std::collections::HashMap;

// Define the main function.
fn main() {
    // Get the current working directory.
    let cwd = std::env::current_dir().unwrap();

    // Create a hash map to store the files and their sizes.
    let mut files: HashMap<String, u64> = HashMap::new();

    // Iterate over the files in the current working directory.
    for entry in fs::read_dir(cwd).unwrap() {
        // Get the file path.
        let file_path = entry.unwrap().path();

        // Get the file name.
        let file_name = file_path.file_name().unwrap().to_str().unwrap();

        // Get the file size.
        let file_size = file_path.metadata().unwrap().len();

        // Add the file and its size to the hash map.
        files.insert(file_name.to_string(), file_size);
    }

    // Create a file to write the results to.
    let mut file = fs::File::create("files.txt").unwrap();

    // Write the header to the file.
    file.write_all(b"File Name,File Size\n").unwrap();

    // Iterate over the files and write their names and sizes to the file.
    for (file_name, file_size) in files.iter() {
        let line = format!("{},{}\n", file_name, file_size);
        file.write_all(line.as_bytes()).unwrap();
    }

    // Flush the file.
    file.flush().unwrap();
}
```

This code is a Rust program that gets the files and their sizes in the current working directory and writes them to a file. The code uses the `fs` library to interact with the file system, the `io` library to write to the file, and the `path` library to work with file paths.

The main function first gets the current working directory using the `std::env::current_dir()` function. Then, it creates a hash map to store the files and their sizes. The hash map is a data structure that maps keys to values. In this case, the keys are the file names and the values are the file sizes.

Next, the code iterates over the files in the current working directory using the `fs::read_dir()` function. For each file, it gets the file path, the file name, and the file size. Then, it adds the file and its size to the hash map.

After iterating over all the files in the current working directory, the code creates a file to write the results to using the `fs::File::create()` function. Then, it writes the header to the file. The header contains the column names for the file name and the file size.

Next, the code iterates over the files and writes their names and sizes to the file. The `format!()` function is used to format the line of text that is written to the file. The `as_bytes()` function is used to convert the string to a byte array, which is required by the `write_all()` function.

Finally, the code flushes the file to ensure that all the data is written to the file.