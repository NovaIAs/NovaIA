```elixir
defmodule ComplexElixirCode do
  def translate_pig_latin(sentence) do
    sentence
    |> String.split()
    |> Enum.map(fun translate_word/1)
    |> Enum.join(" ")
  end

  defp translate_word(word) do
    cond do
      String.match?(word, ~r/^([^aeiouy]*)(.*)/) -> String.replace(word, ~r/^([^aeiouy]*)(.*)/, "\\2\\1ay")
      true -> "#{word}yay"
    end
  end

  def calculate_fibonacci(n) when n <= 1, do: n
  def calculate_fibonacci(n), do: calculate_fibonacci(n - 1) + calculate_fibonacci(n - 2)

  defmodule TreeNode do
    defstruct [:value, :left, :right]
  end

  defmodule BinarySearchTree do
    def insert(tree, value) do
      case tree do
        nil -> %TreeNode{value: value}
        %TreeNode{value: root_value} ->
          if value < root_value, do: %TreeNode{value: root_value, left: insert(tree.left, value)},
          else: %TreeNode{value: root_value, right: insert(tree.right, value)}
      end
    end

    def search(tree, value) do
      case tree do
        nil -> false
        %TreeNode{value: root_value} ->
          cond do
            value == root_value -> true
            value < root_value -> search(tree.left, value)
            true -> search(tree.right, value)
          end
      end
    end
  end

  defmodule Graph do
    defstruct [:nodes, :edges]

    def new() do
      %Graph{nodes: [], edges: []}
    end

    def add_node(graph, node) do
      %Graph{graph | nodes: [node | graph.nodes]}
    end

    def add_edge(graph, from_node, to_node) do
      %Graph{graph | edges: [{from_node, to_node} | graph.edges]}
    end

    def find_path(graph, start_node, end_node) do
      visited = MapSet.new()
      find_path_helper(graph, start_node, end_node, visited)
    end

    defp find_path_helper(graph, current_node, end_node, visited) do
      if current_node == end_node, do: [current_node],
      else ->
        visited = MapSet.put(visited, current_node)
        Enum.flat_map(graph.edges, fn {from_node, to_node} ->
          if from_node == current_node and not MapSet.member?(visited, to_node) do
            path = find_path_helper(graph, to_node, end_node, visited)
            if path, do: [current_node | path], else: []
          else
            []
          end
        end)
      end
    end
  end

  defmodule CurrencyConverter do
    defstruct [:exchange_rates]

    def new() do
      %CurrencyConverter{exchange_rates: %{
        "USD" => 1.0,
        "EUR" => 0.85,
        "GBP" => 0.75,
        "JPY" => 110.0
      }}
    end

    def convert(converter, amount, from_currency, to_currency) do
      rate_from = converter.exchange_rates[from_currency]
      rate_to = converter.exchange_rates[to_currency]
      amount * (rate_to / rate_from)
    end
  end
end
```
**Explanation:**
1. **Pig Latin Translator:** Translates an English sentence into Pig Latin.
2. **Fibonacci Calculator:** Calculates the nth Fibonacci number.
3. **Binary Search Tree:** Implements a binary search tree with insert and search operations.
4. **Graph:** Represents a graph with nodes and edges, and provides a depth-first search algorithm to find paths between nodes.
5. **Currency Converter:** Converts currency amounts between different currencies using exchange rates.

This code demonstrates various functionalities and algorithms, showing the versatility of Elixir as a programming language.