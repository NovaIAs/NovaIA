```
// Import required crates
use rand::prelude::*;
use std::io::{self, Write};
use std::thread;
use std::time::Duration;

// Define the possible choices for the computer
const CHOICES: [&str; 3] = ["Rock", "Paper", "Scissors"];

// Define an enum to represent the possible outcomes of a game
#[derive(Clone, Copy, PartialEq, Eq)]
enum Outcome {
    Win,
    Lose,
    Tie,
}

// Define a function to get a random choice from the computer
fn get_computer_choice() -> String {
    let mut rng = rand::thread_rng();
    let choice = rng.gen_range(0..CHOICES.len());
    CHOICES[choice].to_string()
}

// Define a function to get the user's choice
fn get_user_choice() -> String {
    let mut input = String::new();
    io::stdin()
        .read_line(&mut input)
        .expect("Failed to read user input");
    input.trim().to_string()
}

// Define a function to determine the outcome of a game
fn determine_outcome(user_choice: &str, computer_choice: &str) -> Outcome {
    match (user_choice, computer_choice) {
        ("Rock", "Paper") | ("Paper", "Scissors") | ("Scissors", "Rock") => Outcome::Lose,
        ("Rock", "Scissors") | ("Paper", "Rock") | ("Scissors", "Paper") => Outcome::Win,
        _ => Outcome::Tie,
    }
}

// Define a function to play a game of Rock, Paper, Scissors
fn play_game() -> Outcome {
    println!("Rock, Paper, Scissors!");
    println!("Enter your choice (Rock/Paper/Scissors):");

    let user_choice = get_user_choice();
    let computer_choice = get_computer_choice();

    println!("You chose: {}", user_choice);
    println!("Computer chose: {}", computer_choice);

    determine_outcome(&user_choice, &computer_choice)
}

// Define a function to play a series of games
fn play_series(num_games: usize) -> (usize, usize, usize) {
    let mut wins = 0;
    let mut losses = 0;
    let mut ties = 0;

    for _ in 0..num_games {
        match play_game() {
            Outcome::Win => wins += 1,
            Outcome::Lose => losses += 1,
            Outcome::Tie => ties += 1,
        }
    }

    (wins, losses, ties)
}

// Define the main function
fn main() {
    // Create a loop to play multiple series of games
    loop {
        println!("How many games do you want to play? (Enter a number or 'q' to quit):");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read user input");

        let input = input.trim();

        // Check if the user entered 'q' to quit
        if input == "q" {
            break;
        }

        // Convert the input to a number
        let num_games = input.parse::<usize>().expect("Invalid input");

        // Play a series of games and get the results
        let (wins, losses, ties) = play_series(num_games);

        // Display the results
        println!("Wins: {}", wins);
        println!("Losses: {}", losses);
        println!("Ties: {}", ties);

        // Ask the user if they want to play again
        println!("Do you want to play again? (y/n)");

        let mut input = String::new();
        io::stdin()
            .read_line(&mut input)
            .expect("Failed to read user input");

        let input = input.trim();

        // Check if the user entered 'y' to play again
        if input != "y" {
            break;
        }
    }

    println!("Thanks for playing!");
}
```

**Explanation:**

This code implements a text-based game of Rock, Paper, Scissors in Rust. Here's a detailed explanation:

1. **Importing Crates:**
   - `rand::prelude::*`: Imports all items from the `rand` crate, which is used for generating random numbers.
   - `std::io::{self, Write}`: Imports the standard input/output library and the `Write` trait, which is used for writing to the console.
   - `std::thread`: Imports the `thread` module, which is used for creating threads.
   - `std::time::Duration`: Imports the `Duration` type, which is used for representing durations of time.

2. **Defining Constants:**
   - `CHOICES`: An array of strings representing the three possible choices (Rock, Paper, Scissors) for both the user and the computer.

3. **Defining an Enum:**
   - `Outcome`: An enum representing the possible outcomes of a game (Win, Lose, Tie).

4. **Defining Functions:**
   - `get_computer_choice()`: Generates a random choice for the computer from the `CHOICES` array.
   - `get_user_choice()`: Gets the user's choice from the console.
   - `determine_outcome()`: Determines the outcome of a game given the user's choice and the computer's choice.
   - `play_game()`: Plays a single game of Rock, Paper, Scissors and returns the outcome.
   - `play_series()`: Plays a series of games and returns the total wins, losses, and ties.

5. **Main Function:**
   - Creates a loop to play multiple series of games until the user quits.
   - Prompts the user to enter the number of games to play.
   - Plays a series of games and displays the results.
   - Asks the user if they want to play again.

The code uses the `rand` crate to generate random choices for the computer, and it uses the standard input/output library to interact with the user. The game is played in a loop, and the user can choose to play as many series of games as they want. After each series, the program displays the total wins, losses, and ties.