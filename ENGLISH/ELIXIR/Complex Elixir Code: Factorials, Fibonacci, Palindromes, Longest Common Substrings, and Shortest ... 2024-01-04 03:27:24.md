```elixir
defmodule ComplexElixirCode do
  # Define a function to calculate the factorial of a number
  def factorial(n) do
    if n == 0 do
      1
    else
      n * factorial(n-1)
    end
  end

  # Define a function to generate a list of Fibonacci numbers
  def fibonacci(n) do
    if n <= 1 do
      [n]
    else
      [0, 1] ++ fibonacci(n-1) ++ [fibonacci(n-1) + fibonacci(n-2)]
    end
  end

  # Define a function to check if a string is a palindrome
  def is_palindrome?(string) do
    string == String.reverse(string)
  end

  # Define a function to find the longest common substring of two strings
  def longest_common_substring(string1, string2) do
    # Convert the strings to lists of characters
    list1 = String.codepoints(string1)
    list2 = String.codepoints(string2)

    # Find the longest common substring using dynamic programming
    lcs = ""
    max_length = 0

    for i <- 0..(length(list1)-1) do
      for j <- 0..(length(list2)-1) do
        if list1[i] == list2[j] do
          # Extend the current longest common substring
          lcs = lcs <> list1[i]

          # Update the maximum length
          max_length = max(max_length, length(lcs))
        else
          # Reset the current longest common substring
          lcs = ""
        end
      end
    end

    lcs
  end

  # Define a function to find the shortest path between two nodes in a graph
  def shortest_path(graph, source, destination) do
    # Initialize the distance of all nodes to infinity
    distances = Enum.map(graph.nodes, fn node -> {node, :infinity} end)

    # Set the distance of the source node to 0
    distances = Map.put(distances, source, 0)

    # Initialize the queue of nodes to visit
    queue = [source]

    # While there are nodes to visit
    while queue != [] do
      # Get the next node to visit
      node = Enum.at(queue, 0)

      # Remove the node from the queue
      queue = Enum.drop(queue, 1)

      # For each neighbor of the node
      for neighbor <- graph.neighbors(node) do
        # Calculate the distance to the neighbor
        distance = distances[node] + graph.edge_weight(node, neighbor)

        # If the distance to the neighbor is shorter than the current distance
        if distance < distances[neighbor] do
          # Update the distance to the neighbor
          distances = Map.put(distances, neighbor, distance)

          # Add the neighbor to the queue
          queue = queue ++ [neighbor]
        end
      end
    end

    # Return the distance to the destination node
    distances[destination]
  end
end
```

Explanation:

This Elixir code defines a module called `ComplexElixirCode` that contains five functions:

1. `factorial/1`: This function calculates the factorial of a given number. Factorial is the product of all positive integers less than or equal to the given number. For example, `factorial(5)` would return 120 (5 * 4 * 3 * 2 * 1).

2. `fibonacci/1`: This function generates a list of Fibonacci numbers up to the given number. The Fibonacci sequence is a series of numbers where each number is the sum of the two preceding numbers. The first two numbers in the sequence are 0 and 1. For example, `fibonacci(10)` would return `[0, 1, 1, 2, 3, 5, 8, 13, 21, 34]`.

3. `is_palindrome?/1`: This function checks if a given string is a palindrome. A palindrome is a string that reads the same forward and backward. For example, `is_palindrome?("racecar")` would return `true`, while `is_palindrome?("hello")` would return `false`.

4. `longest_common_substring/2`: This function finds the longest common substring of two given strings. The longest common substring is the longest string that occurs as a subsequence in both strings. For example, `longest_common_substring("ABCD", "ABEFG")` would return `"AB"`.

5. `shortest_path/3`: This function finds the shortest path between two nodes in a weighted graph. The graph is represented as a map of nodes to their neighbors and edge weights. The shortest path is the path with the smallest total weight. For example, if we have a graph with nodes `A`, `B`, `C`, and `D`, and edges `A-B` with weight 5, `B-C` with weight 10, `C-D` with weight 15, and `A-D` with weight 20, then `shortest_path(graph, "A", "D")` would return 25 (A-D).

These functions demonstrate the use of various Elixir features such as pattern matching, recursion, list comprehensions, and map manipulation. They also cover a wide range of topics, from basic mathematical operations to graph theory.