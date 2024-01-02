Certainly! Here's a complex and differentiated JavaScript code that generates a random maze using a depth-first search algorithm. The code is well-commented to explain its functionality.

```javascript
// Function to generate a random maze using Depth-First Search algorithm
function generateMaze(rows, columns) {
  // Create a 2D array to represent the maze
  const maze = [];
  for (let i = 0; i < rows; i++) {
    maze[i] = [];
    for (let j = 0; j < columns; j++) {
      maze[i][j] = {
        visited: false, // Track if the cell is visited or not
        walls: {
          top: true, // Track if the top wall exists or not
          right: true, // Track if the right wall exists or not
          bottom: true, // Track if the bottom wall exists or not
          left: true, // Track if the left wall exists or not
        },
      };
    }
  }

  // Function to get neighboring cells of a given cell
  function getNeighbors(cell) {
    const { row, col } = cell;
    const neighbors = [];

    // Check if the neighboring cells are within the maze boundaries
    if (row > 0) neighbors.push(maze[row - 1][col]); // Top cell
    if (col < columns - 1) neighbors.push(maze[row][col + 1]); // Right cell
    if (row < rows - 1) neighbors.push(maze[row + 1][col]); // Bottom cell
    if (col > 0) neighbors.push(maze[row][col - 1]); // Left cell

    // Shuffle the neighbors randomly
    for (let i = neighbors.length - 1; i > 0; i--) {
      const j = Math.floor(Math.random() * (i + 1));
      [neighbors[i], neighbors[j]] = [neighbors[j], neighbors[i]];
    }

    return neighbors;
  }

  // Function to remove walls between two cells
  function removeWalls(currentCell, nextCell) {
    const rowDiff = currentCell.row - nextCell.row;
    const colDiff = currentCell.col - nextCell.col;

    if (rowDiff === 1) {
      currentCell.walls.top = false;
      nextCell.walls.bottom = false;
    } else if (rowDiff === -1) {
      currentCell.walls.bottom = false;
      nextCell.walls.top = false;
    } else if (colDiff === 1) {
      currentCell.walls.left = false;
      nextCell.walls.right = false;
    } else if (colDiff === -1) {
      currentCell.walls.right = false;
      nextCell.walls.left = false;
    }
  }

  // Function to perform Depth-First Search
  function depthFirstSearch(currentCell) {
    currentCell.visited = true;

    const neighbors = getNeighbors(currentCell);
    for (const neighbor of neighbors) {
      if (!neighbor.visited) {
        removeWalls(currentCell, neighbor);
        depthFirstSearch(neighbor);
      }
    }
  }

  // Start generating the maze from the top-left cell
  const startCell = maze[0][0];
  depthFirstSearch(startCell);

  return maze;
}

// Example usage
const mazeRows = 10; // Number of rows in the maze
const mazeColumns = 10; // Number of columns in the maze

const maze = generateMaze(mazeRows, mazeColumns);
console.log(maze);
```

This code generates a random maze using a depth-first search algorithm. The maze is represented as a 2D array of cells, where each cell has properties to track if it has been visited and if its walls exist or not.

The `generateMaze` function takes the number of rows and columns as parameters and returns the generated maze. It initializes the maze grid, creates the necessary helper functions, and starts the depth-first search algorithm from the top-left cell.

The `depthFirstSearch` function recursively visits each cell's unvisited neighbors, removes the walls between them, and continues the search until all cells have been visited.

Finally, an example usage is provided to generate a maze with 10 rows and 10 columns, which is then logged to the console.

Feel free to modify the `mazeRows` and `mazeColumns` variables to customize the size of the maze. Enjoy exploring the randomly generated mazes!