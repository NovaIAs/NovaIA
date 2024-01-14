```elixir
defmodule Factorial do
  def fact(n) when n == 0, do: 1
  def fact(n) when n > 0, do: n * fact(n - 1)
end

defmodule Fibonacci do
  def fib(n) when n == 0, do: 0
  def fib(n) when n == 1, do: 1
  def fib(n) when n > 1, do: fib(n - 1) + fib(n - 2)
end

defmodule Prime do
  def is_prime(n) when n < 2, do: false
  def is_prime(n), do: is_prime(n, 2)

  defp is_prime(n, d) when d * d > n, do: true
  defp is_prime(n, d) when rem(n, d) == 0, do: false
  defp is_prime(n, d), do: is_prime(n, d + 1)
end

defmodule Sorting do
  def merge_sort(list) do
    case Enum.count(list) do
      0, do: list
      1, do: list
      _, do: merge(merge_sort(Enum.take(list, div(Enum.count(list), 2))), merge_sort(Enum.drop(list, div(Enum.count(list), 2))))
    end
  end

  defp merge(left, right) do
    case {left, right} do
      {[], _}, do: right
      {_, []}, do: left
      {h1 | t1}, {h2 | t2} when h1 < h2, do: [h1 | merge(t1, right)]
      {h1 | t1}, {h2 | t2}, do: [h2 | merge(left, t2)]
    end
  end
end

defmodule BinarySearch do
  def binary_search(list, target) do
    binary_search(list, target, 0, Enum.count(list) - 1)
  end

  defp binary_search([], _target, _low, _high), do: -1

  defp binary_search(list, target, low, high) do
    mid = div(low + high, 2)
    case list[mid] do
      ^target, do: mid
      x when x < target, do: binary_search(list, target, mid + 1, high)
      _x, do: binary_search(list, target, low, mid - 1)
    end
  end
end

defmodule Tree do
  def insert(tree, value) do
    case tree do
      nil, do: {value, nil, nil}
      {v, left, right} when value < v, do: {v, insert(left, value), right}
      {v, left, right}, do: {v, left, insert(right, value)}
    end
  end

  def search(tree, value) do
    case tree do
      nil, do: false
      {v, left, right} when value == v, do: true
      {v, left, right} when value < v, do: search(left, value)
      {v, left, right}, do: search(right, value)
    end
  end
end

defmodule Graph do
  def dijkstra(graph, start) do
    distances = Enum.map(graph, fn {_, _} -> {:inf, nil} end) |> Map.put(start, {0, nil})
    visited = MapSet.new()

    while not MapSet.member?(visited, start) do
      {current, distance} = Enum.min_by(distances, fn {_, v} -> v end)
      visited = MapSet.put(visited, current)

      Enum.each(graph[current], fn {neighbor, weight} ->
        new_distance = distance + weight
        if new_distance < distances[neighbor] do
          distances = Map.put(distances, neighbor, {new_distance, current})
        end
      end)
    end

    distances
  end
end
```