Certainly! Here's a complex TypeScript code that generates a random maze using the Recursive Backtracking algorithm:

```typescript
type Cell = {
  x: number;
  y: number;
  visited: boolean;
  walls: {
    top: boolean;
    right: boolean;
    bottom: boolean;
    left: boolean;
  };
};

class Maze {
  private readonly rows: number;
  private readonly cols: number;
  private readonly grid: Cell[][];
  private readonly stack: Cell[];

  constructor(rows: number, cols: number) {
    this.rows = rows;
    this.cols = cols;
    this.grid = this.initializeGrid();
    this.stack = [];
  }

  private initializeGrid(): Cell[][] {
    const grid: Cell[][] = [];

    for (let i = 0; i < this.rows; i++) {
      grid[i] = [];
      for (let j = 0; j < this.cols; j++) {
        grid[i][j] = {
          x: i,
          y: j,
          visited: false,
          walls: {
            top: true,
            right: true,
            bottom: true,
            left: true,
          },
        };
      }
    }

    return grid;
  }

  private getRandomNeighbor(cell: Cell): Cell | undefined {
    const neighbors: Cell[] = [];

    const top = this.grid[cell.x]?.[cell.y - 1];
    const right = this.grid[cell.x + 1]?.[cell.y];
    const bottom = this.grid[cell.x]?.[cell.y + 1];
    const left = this.grid[cell.x - 1]?.[cell.y];

    if (top && !top.visited) neighbors.push(top);
    if (right && !right.visited) neighbors.push(right);
    if (bottom && !bottom.visited) neighbors.push(bottom);
    if (left && !left.visited) neighbors.push(left);

    if (neighbors.length > 0) {
      const randomIndex = Math.floor(Math.random() * neighbors.length);
      return neighbors[randomIndex];
    }

    return undefined;
  }

  private removeWalls(current: Cell, next: Cell): void {
    const x = current.x - next.x;
    const y = current.y - next.y;

    if (x === 1) {
      current.walls.left = false;
      next.walls.right = false;
    } else if (x === -1) {
      current.walls.right = false;
      next.walls.left = false;
    } else if (y === 1) {
      current.walls.top = false;
      next.walls.bottom = false;
    } else if (y === -1) {
      current.walls.bottom = false;
      next.walls.top = false;
    }
  }

  public generate(): Cell[][] {
    const startCell = this.grid[0][0];
    this.stack.push(startCell);
    startCell.visited = true;

    while (this.stack.length > 0) {
      const currentCell = this.stack[this.stack.length - 1];
      const nextCell = this.getRandomNeighbor(currentCell);

      if (nextCell) {
        nextCell.visited = true;
        this.removeWalls(currentCell, nextCell);
        this.stack.push(nextCell);
      } else {
        this.stack.pop();
      }
    }

    return this.grid;
  }
}

// Example usage:
const maze = new Maze(10, 10);
const grid = maze.generate();

console.log(grid);
```

This code defines a `Maze` class that generates a 2D grid of cells. Each cell has properties indicating its position (`x` and `y`), whether it has been visited, and whether it has walls on its four sides.

The `initializeGrid` method creates the initial grid with all cells unvisited and with all walls intact. The `getRandomNeighbor` method retrieves a random unvisited neighbor of a given cell. The `removeWalls` method removes the walls between the current and next cells.

The `generate` method implements the Recursive Backtracking algorithm to generate the maze. It starts at the top-left cell, marks it as visited, and repeatedly selects a random unvisited neighbor until there are no more unvisited neighbors. If a neighbor is found, it marks it as visited, removes the walls between the current and next cells, and adds the next cell to the stack. If no neighbor is found, it pops the last cell from the stack.

Finally, an example usage is provided, creating a maze with a size of 10 rows and 10 columns, and printing the resulting grid to the console.

Please note that this is a simplified implementation and there are many variations and optimizations that can be applied to maze generation algorithms.