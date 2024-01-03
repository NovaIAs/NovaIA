Certainly! Here's a complex and differentiated Ruby code that generates a random maze using the Depth-First Search algorithm. The code is well-commented to explain the logic behind each step:

```ruby
class Maze
  def initialize(rows, columns)
    @rows = rows
    @columns = columns
    @maze = Array.new(@rows) { Array.new(@columns, '#') } # Initialize the maze with walls
    @start = [0, 0] # Starting position
    @end = [@rows - 1, @columns - 1] # Ending position
    generate_maze
  end

  def generate_maze
    stack = [] # Stack to keep track of visited cells during the maze generation
    current = @start # Start building the maze from the starting position
    @maze[current[0]][current[1]] = ' ' # Mark the starting position as a path

    loop do
      neighbors = valid_neighbors(current) # Get the valid neighbors of the current cell

      if neighbors.empty?
        break if stack.empty? # Maze generation is complete when the stack is empty

        current = stack.pop # Backtrack to the previous cell
      else
        neighbor = neighbors.sample # Choose a random neighbor
        x = (current[0] + neighbor[0]) / 2 # Calculate the x-coordinate between current and neighbor
        y = (current[1] + neighbor[1]) / 2 # Calculate the y-coordinate between current and neighbor
        @maze[x][y] = ' ' # Break the wall between current and neighbor
        @maze[neighbor[0]][neighbor[1]] = ' ' # Mark the neighbor as a path
        stack.push(current) # Push current cell to the stack
        current = neighbor # Move to the neighbor cell
      end
    end
  end

  def valid_neighbors(cell)
    neighbors = []

    # Check the top neighbor
    if cell[0] - 2 >= 0 && @maze[cell[0] - 2][cell[1]] == '#'
      neighbors << [cell[0] - 2, cell[1]]
    end

    # Check the right neighbor
    if cell[1] + 2 < @columns && @maze[cell[0]][cell[1] + 2] == '#'
      neighbors << [cell[0], cell[1] + 2]
    end

    # Check the bottom neighbor
    if cell[0] + 2 < @rows && @maze[cell[0] + 2][cell[1]] == '#'
      neighbors << [cell[0] + 2, cell[1]]
    end

    # Check the left neighbor
    if cell[1] - 2 >= 0 && @maze[cell[0]][cell[1] - 2] == '#'
      neighbors << [cell[0], cell[1] - 2]
    end

    neighbors
  end

  def display_maze
    @maze.each { |row| puts row.join('') }
  end
end

# Create a 15x15 maze and display it
maze = Maze.new(15, 15)
maze.display_maze
```

The code defines a `Maze` class in which the `initialize` method sets up the basic structure of the maze by creating a 2D array (`@maze`) filled with walls (`'#'`). The starting position is set at `[0, 0]` and the ending position at `[@rows - 1, @columns - 1]`.

The maze generation process is performed by the `generate_maze` method, implementing the Depth-First Search algorithm. It uses a stack to keep track of visited cells. Starting from the beginning, it randomly selects a neighbor cell and breaks the wall between the current and chosen neighbor cells. The current cell is then updated to the chosen neighbor cell, and the process continues until there are no more valid neighbors. At this point, the algorithm backtracks by popping the previous cell from the stack and repeats the process until the stack is empty.

The `valid_neighbors` method checks the neighboring cells of a given cell and returns an array containing the valid neighbors.

Finally, the `display_maze` method prints the generated maze on the console.

To run the code, create an instance of the `Maze` class with the desired number of rows and columns (e.g., `maze = Maze.new(15, 15)`), and then call the `display_maze` method (`maze.display_maze`) to see the generated maze.