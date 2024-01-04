```rust
// This is a Rust program that implements a simple shell.

use std::io::{self, BufRead, Write};
use std::process::{Command, Stdio};

fn main() {
    // Create a new shell instance.
    let mut shell = Shell::new();

    // Run the shell loop.
    loop {
        // Print the shell prompt.
        print!("$ ");
        io::stdout().flush().unwrap();

        // Read the user's input.
        let mut input = String::new();
        io::stdin().read_line(&mut input).unwrap();

        // Parse the user's input.
        let mut args = input.split_whitespace();
        let command = args.next().unwrap();

        // Execute the command.
        match command {
            "cd" => {
                // Change the current directory.
                let new_dir = args.next().unwrap();
                shell.change_directory(new_dir);
            }
            "ls" => {
                // List the files in the current directory.
                shell.list_files();
            }
            "exit" => {
                // Exit the shell.
                break;
            }
            _ => {
                // Execute the command as a program.
                let mut child = Command::new(command)
                    .args(args)
                    .stdout(Stdio::inherit())
                    .stderr(Stdio::inherit())
                    .spawn()
                    .unwrap();

                child.wait().unwrap();
            }
        }
    }
}

// The Shell struct represents a shell instance.
struct Shell {
    current_directory: String,
}

// The Shell::new function creates a new shell instance.
impl Shell {
    fn new() -> Shell {
        Shell {
            current_directory: std::env::current_dir().unwrap().into_os_string().into_string().unwrap(),
        }
    }

    // The Shell::change_directory function changes the current directory.
    fn change_directory(&mut self, new_dir: &str) {
        std::env::set_current_dir(new_dir).unwrap();
        self.current_directory = std::env::current_dir().unwrap().into_os_string().into_string().unwrap();
    }

    // The Shell::list_files function lists the files in the current directory.
    fn list_files(&self) {
        for entry in std::fs::read_dir(&self.current_directory).unwrap() {
            println!("{}", entry.unwrap().file_name().into_string().unwrap());
        }
    }
}
```

This code implements a simple shell in Rust. The shell has three built-in commands: `cd`, `ls`, and `exit`. The `cd` command changes the current directory, the `ls` command lists the files in the current directory, and the `exit` command exits the shell.

The shell also allows users to execute other programs as commands. When a user enters a command that is not a built-in command, the shell will try to execute the command as a program.

The shell uses the `Command` struct to execute commands. The `Command` struct allows the user to specify the command to be executed, the arguments to the command, and the standard input, standard output, and standard error streams of the command.

The `Command` struct also has a `wait` method, which blocks until the command has finished executing. The `wait` method returns a `Result` value, which contains the exit code of the command.

The shell uses the `std::io::BufRead` trait to read the user's input. The `BufRead` trait allows the user to read a line of input from a stream.

The shell uses the `std::io::Write` trait to print the shell prompt and the results of commands. The `Write` trait allows the user to write data to a stream.

The shell uses the `std::env::current_dir` function to get the current directory. The `std::env::current_dir` function returns a `Result` value, which contains the current directory as a `PathBuf` value.

The shell uses the `std::env::set_current_dir` function to change the current directory. The `std::env::set_current_dir` function takes a `PathBuf` value as an argument and changes the current directory to the specified directory.

The shell uses the `std::fs::read_dir` function to list the files in a directory. The `std::fs::read_dir` function takes a directory path as an argument and returns a `Result` value, which contains an iterator over the files in the directory.

The shell uses the `std::fs::File` struct to read the contents of a file. The `std::fs::File` struct allows the user to read and write data from a file.

The shell uses the `std::io::BufReader` struct to read the contents of a file line by line. The `std::io::BufReader` struct allows the user to read data from a stream line by line.