Certainly! Here's a complex and differentiated Java code that generates a random maze using the Depth-First Search algorithm.

```java
import java.util.*;

public class MazeGenerator {
    private final int rows;
    private final int cols;
    private final int[][] maze;
    private final Random random;

    public MazeGenerator(int rows, int cols) {
        this.rows = rows;
        this.cols = cols;
        maze = new int[rows][cols];
        random = new Random();
    }

    public void generateMaze() {
        initializeMaze();
        generatePath(0, 0);
    }

    private void initializeMaze() {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                maze[i][j] = 1;
            }
        }
    }

    private void generatePath(int row, int col) {
        maze[row][col] = 0;
        List<Direction> directions = Arrays.asList(Direction.values());
        Collections.shuffle(directions, random);

        for (Direction direction : directions) {
            int newRow = row + direction.getRow();
            int newCol = col + direction.getCol();

            if (isValidCell(newRow, newCol) && maze[newRow][newCol] == 1) {
                int wallRow = row + direction.getWallRow();
                int wallCol = col + direction.getWallCol();

                maze[wallRow][wallCol] = 0;
                generatePath(newRow, newCol);
            }
        }
    }

    private boolean isValidCell(int row, int col) {
        return row >= 0 && row < rows && col >= 0 && col < cols;
    }

    public void printMaze() {
        for (int i = 0; i < rows; i++) {
            for (int j = 0; j < cols; j++) {
                System.out.print(maze[i][j] == 0 ? "  " : "██");
            }
            System.out.println();
        }
    }

    private enum Direction {
        UP(-1, 0, -1, 0),
        DOWN(1, 0, 0, 0),
        LEFT(0, -1, 0, -1),
        RIGHT(0, 1, 0, 0);

        private final int row;
        private final int col;
        private final int wallRow;
        private final int wallCol;

        Direction(int row, int col, int wallRow, int wallCol) {
            this.row = row;
            this.col = col;
            this.wallRow = wallRow;
            this.wallCol = wallCol;
        }

        public int getRow() {
            return row;
        }

        public int getCol() {
            return col;
        }

        public int getWallRow() {
            return wallRow;
        }

        public int getWallCol() {
            return wallCol;
        }
    }

    public static void main(String[] args) {
        MazeGenerator mazeGenerator = new MazeGenerator(10, 20);
        mazeGenerator.generateMaze();
        mazeGenerator.printMaze();
    }
}
```

Explanation: This Java code generates a maze using the Depth-First Search algorithm. It starts by initializing a 2D array called `maze` with all walls (represented by 1s). Then, it recursively generates a path through the maze by breaking walls between adjacent cells. The algorithm begins at the top-left cell (0, 0) and randomly chooses a direction to move. If the new cell is valid and unvisited, it breaks the wall between the current cell and the new cell, marks the new cell as visited, and repeats the process for the new cell. This continues until all cells have been visited. Finally, the `printMaze()` method prints the maze on the console, displaying walls as "██" and paths as empty spaces.