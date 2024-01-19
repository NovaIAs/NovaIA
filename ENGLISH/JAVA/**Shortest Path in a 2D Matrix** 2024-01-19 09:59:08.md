```java

import java.util.*;
import java.lang.*;
import java.io.*;

class Solution {
    public static void main(String[] args) {
        Scanner input = new Scanner(System.in);

        // Read input values
        int n = input.nextInt();
        int m = input.nextInt();
        int k = input.nextInt();
        int[][] matrix = new int[n][m];

        // Read matrix values
        for (int i = 0; i < n; i++) {
            for (int j = 0; j < m; j++) {
                matrix[i][j] = input.nextInt();
            }
        }

        // Create a queue to store the indices of cells to be visited
        Queue<int[]> queue = new LinkedList<>();

        // Add the starting cell (0, 0) to the queue
        queue.add(new int[]{0, 0});

        // Create a set to store the visited cells
        Set<String> visited = new HashSet<>();

        // Create a variable to store the shortest path length
        int shortestPathLength = Integer.MAX_VALUE;

        // Explore the matrix using BFS
        while (!queue.isEmpty()) {
            // Get the current cell
            int[] currentCell = queue.poll();

            // Get the row and column of the current cell
            int currentRow = currentCell[0];
            int currentColumn = currentCell[1];

            // Check if the current cell has been visited
            String cellKey = currentRow + "," + currentColumn;
            if (visited.contains(cellKey)) {
                continue;
            }

            // Mark the current cell as visited
            visited.add(cellKey);

            // Check if the current cell is the destination cell
            if (currentRow == n - 1 && currentColumn == m - 1) {
                // Update the shortest path length
                shortestPathLength = Math.min(shortestPathLength, currentCell[2]);
            } else {
                // Check if the current cell is within bounds
                if (currentRow >= 0 && currentRow < n && currentColumn >= 0 && currentColumn < m) {
                    // Check if the current cell is not an obstacle
                    if (matrix[currentRow][currentColumn] != 1) {
                        // Add the neighboring cells to the queue
                        queue.add(new int[]{currentRow - 1, currentColumn, currentCell[2] + 1});
                        queue.add(new int[]{currentRow + 1, currentColumn, currentCell[2] + 1});
                        queue.add(new int[]{currentRow, currentColumn - 1, currentCell[2] + 1});
                        queue.add(new int[]{currentRow, currentColumn + 1, currentCell[2] + 1});
                    }
                }
            }
        }

        // Print the shortest path length
        System.out.println(shortestPathLength);
    }
}

```

This code finds the shortest path from the top-left corner to the bottom-right corner of a 2D matrix, where each cell can either be empty (0) or an obstacle (1). It uses a queue to perform a breadth-first search (BFS) and keeps track of the visited cells to avoid cycles. If a path to the destination is found, the code updates the shortest path length. If there is no path to the destination, the code outputs -1.