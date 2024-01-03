Sure! Here's a complex code in Julia that generates a random maze using the Depth-First Search algorithm:

```julia
using Random

function generate_maze(height, width)
    grid = fill('#', 2*height+1, 2*width+1)
    visited = fill(false, height, width)

    function dfs(row, col)
        visited[row, col] = true
        grid[2*row+1, 2*col+1] = ' '

        directions = [(0, 2), (0, -2), (2, 0), (-2, 0)]
        Random.shuffle!(directions)

        for (dx, dy) in directions
            new_row, new_col = row + dx, col + dy

            if 1 ≤ new_row ≤ height && 1 ≤ new_col ≤ width && !visited[new_row, new_col]
                grid[row + new_row + 1, col + new_col + 1] = ' '
                dfs(new_row, new_col)
            end
        end
    end

    dfs(1, 1)
    grid
end

function print_maze(maze)
    for row in maze
        println(join(row))
    end
end

# Usage example
height = 10
width = 20
maze = generate_maze(height, width)
print_maze(maze)
```

This code generates a random maze by performing a Depth-First Search (DFS) algorithm. It starts by creating a grid filled with walls ('#') and empty spaces (' '). The maze is represented as a 2D array. Then, it randomly selects a starting point and performs a DFS traversal to carve out paths in the maze.

The `generate_maze` function takes two parameters: `height` and `width`, representing the dimensions of the maze. It initializes the grid and visited arrays, and defines the `dfs` inner function. The `dfs` function performs the depth-first search recursively by randomly selecting a direction to move and visiting unvisited cells until there are no more unvisited cells.

The `print_maze` function simply prints the maze grid to the console.

In the usage example, we define the `height` and `width` of the maze and call `generate_maze` to generate a maze of the specified dimensions. Finally, we print the maze using the `print_maze` function.