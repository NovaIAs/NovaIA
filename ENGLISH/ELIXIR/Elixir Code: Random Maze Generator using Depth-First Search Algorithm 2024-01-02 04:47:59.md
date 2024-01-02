Certainly! Here's a complex Elixir code that generates a random maze using a depth-first search algorithm. The code utilizes recursive backtracking to create a unique maze every time it is executed.

```elixir
defmodule MazeGenerator do
  def generate(rows, cols) do
    maze = initialize_maze(rows, cols)
    generate_maze(maze, 0, 0)
    maze
  end

  defp initialize_maze(rows, cols) do
    Enum.map(0..rows-1, fn _ ->
      Enum.map(0..cols-1, fn _ ->
        %{top: true, right: true, bottom: true, left: true, visited: false}
      end)
    end)
  end

  defp generate_maze(maze, row, col) do
    maze[row][col] = Map.put(maze[row][col], :visited, true)
    shuffle_directions |> Enum.each(fn direction ->
      new_row = row + direction[:dy]
      new_col = col + direction[:dx]
      if valid_cell?(maze, new_row, new_col) do
        maze[row][col] = remove_wall(maze[row][col], direction[:wall])
        maze[new_row][new_col] = remove_wall(maze[new_row][new_col], direction[:opposite_wall])

        generate_maze(maze, new_row, new_col)
      end
    end)
  end

  defp valid_cell?(maze, row, col) do
    row >= 0 && row < length(maze) && col >= 0 && col < length(Enum.at(maze, 0)) && not(maze[row][col].visited)
  end

  defp remove_wall(cell, wall) do
    Map.put(cell, wall, false)
  end

  defp shuffle_directions do
    directions = [%{dy: -1, dx: 0, wall: :top, opposite_wall: :bottom},
                  %{dy: 0, dx: 1, wall: :right, opposite_wall: :left},
                  %{dy: 1, dx: 0, wall: :bottom, opposite_wall: :top},
                  %{dy: 0, dx: -1, wall: :left, opposite_wall: :right}]
    Enum.shuffle(directions)
  end
end

# Example usage
maze = MazeGenerator.generate(10, 10)
IO.inspect(maze)
```

Explanation:
1. The `MazeGenerator` module is defined to encapsulate the maze generation logic.
2. The `generate` function is the entry point that takes the number of rows and columns as arguments and returns a generated maze.
3. The `initialize_maze` function creates an empty maze grid filled with cell structs. Each cell has walls on all sides and is initially marked as unvisited.
4. The `generate_maze` function implements the depth-first search algorithm to recursively generate the maze. It starts from the top-left cell and randomly selects a neighboring unvisited cell to move to. As it moves, it removes the walls between the current cell and the chosen cell. The function continues until all cells have been visited.
5. The `valid_cell?` function checks if a given cell coordinate is within the maze boundaries and has not been visited yet.
6. The `remove_wall` function updates a cell struct by setting a specific wall to false, indicating that the wall has been removed.
7. The `shuffle_directions` function creates a list of movement directions (up, right, down, left) and shuffles them randomly for each call. This provides randomness to the maze generation process.
8. Finally, an example usage is shown where a 10x10 maze is generated using `MazeGenerator.generate/2` and printed to the console using `IO.inspect`.