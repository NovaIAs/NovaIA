```elixir
defmodule DoWhileLoop do
  def do_while(function, condition) do
    Value = function.()
    if condition.(Value) do
      do_while(function, condition)
    end
    Value
  end
end

defmodule Factorial do
  def factorial(n) do
    DoWhileLoop.do_while(fn -> n end, fn x -> x > 1 end)
    |> Enum.reduce(1, fn x, acc -> x * acc end)
  end
end

defmodule Fibonacci do
  def fibonacci(n) do
    DoWhileLoop.do_while(fn -> {0, 1} end, fn {_, x} -> x < n end)
    |> Enum.reduce([0], fn {_, x}, acc -> [x | acc] end)
    |> Enum.reverse
  end
end

defmodule PrimeFactors do
  def prime_factors(n) when n <= 1, do: []

  def prime_factors(n) do
    DoWhileLoop.do_while(fn -> n end, fn x -> x > 1 end)
    |> Enum.reduce_while({[], 2}, fn x, {acc, divisor} ->
      if rem(x, divisor) == 0 do
        {:cont, {acc ++ [divisor], divisor}}
      else
        {:cont, {acc, divisor + 1}}
      end
    end)
    |> elem(0)
  end
end

defmodule FilterMapReduce do
  def filter_map_reduce([], _filter, _mapper, acc), do: acc

  def filter_map_reduce([head | tail], filter, mapper, acc) do
    if filter.(head) do
      filter_map_reduce(tail, filter, mapper, mapper.(head) |> List.insert_at(acc, 0))
    else
      filter_map_reduce(tail, filter, mapper, acc)
    end
  end

  def filter_map_reduce(list, filter, mapper) do
    filter_map_reduce(list, filter, mapper, [])
  end
end

defmodule SortBy do
  def sort_by(list, key_function) do
    list
    |> Enum.map(fn x -> {key_function.(x), x} end)
    |> Enum.sort_by(fn {_, x} -> x end)
    |> Enum.map(fn {_, x} -> x end)
  end
end

IO.puts "Factorial of 5: #{Factorial.factorial(5)}"
IO.inspect "Fibonacci sequence up to 10: #{Fibonacci.fibonacci(10)}"
IO.inspect "Prime factors of 12: #{PrimeFactors.prime_factors(12)}"
IO.inspect "Filter, map, and reduce: #{FilterMapReduce.filter_map_reduce([1, 2, 3, 4, 5], fn x -> x > 2 end, fn x -> x * 2 end)}"
IO.inspect "Sort by last character: #{SortBy.sort_by(["apple", "banana", "cherry"], &String.last/1)}"
```

This code is a collection of commonly used functional programming patterns in Elixir. It includes:

* A "do-while" loop implemented using recursion and anonymous functions.
* Factorial and Fibonacci number calculation using the "do-while" loop.
* Prime factorization using a "do-while" loop and pattern matching.
* A generic filter, map, and reduce function that takes a list, a filter function, a mapper function, and an accumulator and returns the result of applying the filter, map, and reduce operations to the list.
* A generic sort-by function that takes a list and a key function and returns a list sorted by the values of the key function applied to each element of the list.

This code demonstrates the power and expressiveness of Elixir's functional programming features and can serve as a useful reference for learning and applying these patterns in your own Elixir code.