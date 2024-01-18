```rust
// This program simulates a simple ecosystem of predators and prey.
// The ecosystem is represented as a grid of cells, each of which can
// contain either a predator, a prey animal, or nothing.
// The predators and prey animals move around the grid according to certain
// rules, and they interact with each other when they occupy the same cell.

// The grid is represented as a two-dimensional array of integers.
// Each element of the array represents the state of the corresponding cell.
// A value of 0 indicates an empty cell, a value of 1 indicates a predator,
// and a value of 2 indicates a prey animal.

// The rules for the simulation are as follows:

// 1. Predators move around the grid randomly.
// 2. Prey animals move around the grid randomly.
// 3. If a predator and a prey animal occupy the same cell, the predator
//    eats the prey animal and the prey animal is removed from the grid.
// 4. If a predator does not eat a prey animal, it loses energy.
// 5. If a predator loses all of its energy, it dies and is removed
//    from the grid.
// 6. If a prey animal does not get eaten, it reproduces and creates a new
//    prey animal.

// The simulation is run for a specified number of iterations.
// At the end of each iteration, the grid is printed to the console.

// The following code implements the simulation.

use std::io;
use std::rand;

// The size of the grid.
const GRID_SIZE: usize = 10;

// The number of iterations to run the simulation for.
const ITERATIONS: usize = 100;

// The initial number of predators and prey animals.
const INITIAL_PREDATORS: usize = 10;
const INITIAL_PREY: usize = 10;

// The energy level of a predator.
const PREDATOR_ENERGY: usize = 10;

// The maximum number of iterations a predator can go without eating before it dies.
const PREDATOR_MAX_ITERATIONS_WITHOUT_EATING: usize = 10;

// The number of iterations a prey animal can go without reproducing.
const PREY_REPRODUCTION_INTERVAL: usize = 10;

// The probability that a predator will move in a given direction.
const PREDATOR_MOVE_PROBABILITY: f64 = 0.5;

// The probability that a prey animal will move in a given direction.
const PREY_MOVE_PROBABILITY: f64 = 0.5;

// The main function.
fn main() {
    // Create the grid.
    let mut grid = [[0; GRID_SIZE]; GRID_SIZE];

    // Initialize the grid with predators and prey animals.
    for _ in 0..INITIAL_PREDATORS {
        let (x, y) = get_random_cell();
        grid[x][y] = 1;
    }
    for _ in 0..INITIAL_PREY {
        let (x, y) = get_random_cell();
        grid[x][y] = 2;
    }

    // Run the simulation for the specified number of iterations.
    for _ in 0..ITERATIONS {
        // Move the predators and prey animals around the grid.
        move_predators(&mut grid);
        move_prey(&mut grid);

        // Interact the predators and prey animals.
        interact_predators_and_prey(&mut grid);

        // Print the grid to the console.
        print_grid(&grid);

        // Sleep for a short period of time.
        io::stdin().read_line(&mut String::new()).unwrap();
    }
}

// Gets a random cell in the grid.
fn get_random_cell() -> (usize, usize) {
    (rand::random::<usize>() % GRID_SIZE, rand::random::<usize>() % GRID_SIZE)
}

// Moves the predators around the grid.
fn move_predators(grid: &mut [[usize; GRID_SIZE]; GRID_SIZE]) {
    for i in 0..GRID_SIZE {
        for j in 0..GRID_SIZE {
            if grid[i][j] == 1 {
                // Get a random direction to move in.
                let direction = rand::random::<f64>();

                // Move the predator in the chosen direction.
                match direction {
                    0.0..=PREDATOR_MOVE_PROBABILITY => {
                        if i > 0 {
                            grid[i][j] = 0;
                            grid[i - 1][j] = 1;
                        }
                    }
                    PREDATOR_MOVE_PROBABILITY..=2.0 * PREDATOR_MOVE_PROBABILITY => {
                        if j > 0 {
                            grid[i][j] = 0;
                            grid[i][j - 1] = 1;
                        }
                    }
                    2.0 * PREDATOR_MOVE_PROBABILITY..=3.0 * PREDATOR_MOVE_PROBABILITY => {