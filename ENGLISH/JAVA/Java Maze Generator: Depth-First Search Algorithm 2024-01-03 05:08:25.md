Certainly! Here's a complex and differentiated Java code that generates a random maze using a Depth-First Search algorithm. This code creates a two-dimensional array representing the maze and prints it out to the console.

```java
import java.util.*;

public class MazeGenerator {
    private final int rows;
    private final int cols;
    private final int[][] maze;

    // Constructor
    public MazeGenerator(int rows, int cols) {
        this.rows = rows;
        this.cols = cols;
        this.maze = new int[rows][cols];
    }

    // Generate the maze
    public void generateMaze() {
        Random random = new Random();
        Stack<Integer> stack = new Stack<>();

        // Start position
        int startX = random.nextInt(rows);
        int startY = random.nextInt(cols);

        // Mark start position as visited
        maze[startX][startY] = 1;

        // Push start position to stack
        stack.push(startX);
        stack.push(startY);

        // Depth-First Search algorithm
        while (!stack.isEmpty()) {
            int y = stack.pop();
            int x = stack.pop();

            // Get random directions
            List<Integer> directions = Arrays.asList(1, 2, 3, 4);
            Collections.shuffle(directions, random);

            for (int direction : directions) {
                int newX = x;
                int newY = y;

                if (direction == 1) {  // Up
                    newY -= 2;
                    if (newY >= 0 && maze[newX][newY] == 0) {
                        maze[newX][newY + 1] = 1;
                        maze[newX][newY] = 1;
                        stack.push(newX);
                        stack.push(newY);
                        break;
                    }
                } else if (direction == 2) {  // Right
                    newX += 2;
                    if (newX < cols && maze[newX][newY] == 0) {
                        maze[newX - 1][newY] = 1;
                        maze[newX][newY] = 1;
                        stack.push(newX);
                        stack.push(newY);
                        break;
                    }
                } else if (direction == 3) {  // Down
                    newY += 2;
                    if (newY < rows && maze[newX][newY] == 0) {
                        maze[newX][newY - 1] = 1;
                        maze[newX][newY] = 1;
                        stack.push(newX);
                        stack.push(newY);
                        break;
                    }
                } else if (direction == 4) {  // Left
                    newX -= 2;
                    if (newX >= 0 && maze[newX][newY] == 0) {
                        maze[newX + 1][newY] = 1;
                        maze[newX][newY] = 1;
                        stack.push(newX);
                        stack.push(newY);
                        break;
                    }
                }
            }
        }
    }

    // Print the maze
    public void printMaze() {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                if (maze[i][j] == 1) {
                    System.out.print("  ");  // Open path
                } else {
                    System.out.print("██");  // Wall
                }
            }
            System.out.println();
        }
    }

    // Main method
    public static void main(String[] args) {
        MazeGenerator mazeGenerator = new MazeGenerator(21, 21);
        mazeGenerator.generateMaze();
        mazeGenerator.printMaze();
    }
}
```

Explanation:
1. The `MazeGenerator` class represents the maze generator and has three fields: `rows` and `cols` to store the dimensions of the maze and `maze`, a two-dimensional array to represent the maze.
2. The constructor initializes the dimensions and creates an empty maze.
3. The `generateMaze()` method uses a Depth-First Search algorithm to generate the maze.
4. It randomly selects a starting position, marks it as visited, and pushes it onto the stack.
5. The algorithm proceeds until the stack is empty.
6. For each position, it randomly selects a direction and checks if it is a valid move.
7. If the move is valid, it updates the maze, marks the new position as visited, and pushes it onto the stack.
8. The `printMaze()` method prints the maze to the console. Walls are represented by "██" and open paths by empty spaces.
9. In the `main()` method, an instance of `MazeGenerator` is created, the maze is generated, and then printed to the console. The dimensions of the maze are set to 21 by 21, but you can modify them as desired.