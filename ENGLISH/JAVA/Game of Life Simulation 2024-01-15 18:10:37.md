```java
import java.util.*;
import java.io.*;

public class ComplexCode {

    private static final int NUM_ROWS = 10;
    private static final int NUM_COLS = 10;
    private static final int NUM_ITERATIONS = 1000;

    private static int[][] grid;
    private static Random random;

    public static void main(String[] args) {
        // Initialize the grid with random values
        grid = new int[NUM_ROWS][NUM_COLS];
        random = new Random();
        for (int i = 0; i < NUM_ROWS; i++) {
            for (int j = 0; j < NUM_COLS; j++) {
                grid[i][j] = random.nextInt(10);
            }
        }

        // Iterate over the grid and apply the game of life rules
        for (int iteration = 0; iteration < NUM_ITERATIONS; iteration++) {
            // Create a temporary grid to store the new values
            int[][] newGrid = new int[NUM_ROWS][NUM_COLS];

            // Iterate over each cell in the grid
            for (int i = 0; i < NUM_ROWS; i++) {
                for (int j = 0; j < NUM_COLS; j++) {
                    // Count the number of living neighbors
                    int numNeighbors = countLivingNeighbors(i, j);

                    // Apply the game of life rules
                    if (numNeighbors < 2 || numNeighbors > 3) {
                        newGrid[i][j] = 0; // Cell dies
                    } else if (numNeighbors == 3) {
                        newGrid[i][j] = 1; // Cell is born
                    } else {
                        newGrid[i][j] = grid[i][j]; // Cell remains unchanged
                    }
                }
            }

            // Copy the new grid to the old grid
            grid = newGrid;

            // Print the grid to the console
            printGrid();
        }
    }

    private static int countLivingNeighbors(int row, int col) {
        int numNeighbors = 0;

        // Iterate over the 8 surrounding cells
        for (int i = row - 1; i <= row + 1; i++) {
            for (int j = col - 1; j <= col + 1; j++) {
                // Check if the cell is within the grid bounds
                if (i >= 0 && i < NUM_ROWS && j >= 0 && j < NUM_COLS) {
                    // Check if the cell is alive
                    if (grid[i][j] == 1) {
                        numNeighbors++;
                    }
                }
            }
        }

        // Subtract 1 from the number of neighbors if the cell is alive
        if (grid[row][col] == 1) {
            numNeighbors--;
        }

        return numNeighbors;
    }

    private static void printGrid() {
        for (int i = 0; i < NUM_ROWS; i++) {
            for (int j = 0; j < NUM_COLS; j++) {
                System.out.print(grid[i][j] + " ");
            }
            System.out.println();
        }
        System.out.println();
    }
}
```

This code implements the Game of Life, a cellular automaton devised by John Conway in 1970. The game is played on a grid of cells, each of which can be either alive or dead. The rules of the game are as follows:

* Any live cell with fewer than two live neighbors dies of isolation.
* Any live cell with two or three live neighbors survives.
* Any live cell with four or more live neighbors dies of overcrowding.
* Any dead cell with exactly three live neighbors becomes a live cell.

The code starts by initializing a grid of cells with random values. It then iterates over the grid and applies the game of life rules to each cell. The number of living neighbors for each cell is counted, and the cell is updated accordingly. The new grid is then printed to the console.

The code uses a number of helper methods to make it more readable and maintainable. The `countLivingNeighbors` method counts the number of living neighbors for a given cell. The `printGrid` method prints the grid to the console.

The Game of Life is a fascinating example of a complex system that can be modeled using simple rules. It has been used to study a variety of phenomena, including population dynamics, evolution, and artificial intelligence.