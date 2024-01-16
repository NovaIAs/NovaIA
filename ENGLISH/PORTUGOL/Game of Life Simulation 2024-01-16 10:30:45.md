```
// This program simulates the famous "Game of Life" by John Conway.
// It creates a 2D grid of cells, each of which can be either alive or dead.
// The rules of the game are as follows:
// 1. Any live cell with fewer than 2 live neighbors dies of isolation.
// 2. Any live cell with 2 or 3 live neighbors lives on to the next generation.
// 3. Any live cell with 4 or more live neighbors dies of overcrowding.
// 4. Any dead cell with exactly 3 live neighbors becomes a live cell.

// Import the necessary libraries.
#include <stdio.h>
#include <stdlib.h>

// Define the size of the grid.
#define SIZE 10

// Create the grid.
int grid[SIZE][SIZE];

// Initialize the grid.
void init_grid() {
  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      grid[i][j] = 0;
    }
  }
}

// Print the grid.
void print_grid() {
  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      printf("%d ", grid[i][j]);
    }
    printf("\n");
  }
}

// Update the grid.
void update_grid() {
  int new_grid[SIZE][SIZE];

  // Loop through each cell in the grid.
  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      // Count the number of live neighbors.
      int live_neighbors = 0;
      for (int k = -1; k <= 1; k++) {
        for (int l = -1; l <= 1; l++) {
          if (i + k >= 0 && i + k < SIZE && j + l >= 0 && j + l < SIZE &&
              !(k == 0 && l == 0) && grid[i + k][j + l] == 1) {
            live_neighbors++;
          }
        }
      }

      // Apply the rules of the game.
      if (grid[i][j] == 1) {
        if (live_neighbors < 2 || live_neighbors > 3) {
          new_grid[i][j] = 0;
        } else {
          new_grid[i][j] = 1;
        }
      } else {
        if (live_neighbors == 3) {
          new_grid[i][j] = 1;
        } else {
          new_grid[i][j] = 0;
        }
      }
    }
  }

  // Copy the new grid to the old grid.
  for (int i = 0; i < SIZE; i++) {
    for (int j = 0; j < SIZE; j++) {
      grid[i][j] = new_grid[i][j];
    }
  }
}

// Main function.
int main() {
  // Initialize the grid.
  init_grid();

  // Print the grid.
  print_grid();

  // Update the grid 100 times.
  for (int i = 0; i < 100; i++) {
    update_grid();
  }

  // Print the grid.
  print_grid();

  return 0;
}
```

This code simulates the "Game of Life" by John Conway. The game is played on a 2D grid of cells, each of which can be either alive or dead. The rules of the game are as follows:

1. Any live cell with fewer than 2 live neighbors dies of isolation.
2. Any live cell with 2 or 3 live neighbors lives on to the next generation.
3. Any live cell with 4 or more live neighbors dies of overcrowding.
4. Any dead cell with exactly 3 live neighbors becomes a live cell.

The code first creates a 2D grid of cells, each of which is initially set to dead. It then prints the grid to the console.

The code then updates the grid 100 times. Each time the grid is updated, the code loops through each cell in the grid and counts the number of live neighbors that it has. The code then applies the rules of the game to each cell, based on the number of live neighbors that it has.

After the grid has been updated 100 times, the code prints the grid to the console again. This allows the user to see how the game has evolved over time.