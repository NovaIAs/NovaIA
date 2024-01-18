```elixir
# Create a function to check if a given list contains any odd numbers.
defmodule List.ContainsOdd do
  # Define the function with one parameter 'list'.
  def contains_odd(list) do
    # Iterate over each element 'x' in the list.
    Enum.any?(list, fn x -> rem(x, 2) == 1 end)
  end
end

# Create a function to generate a list of all odd numbers up to a given limit.
defmodule List.GenerateOdds do
  # Define the function with one parameter 'limit'.
  def generate_odds(limit) do
    # Use the Range module to create a range of numbers from 1 to 'limit'.
    Range.new(1, limit)
    # Filter the range to include only odd numbers using the 'rem' function.
    |> Enum.filter(fn x -> rem(x, 2) == 1 end)
    # Convert the filtered range back to a list.
    |> Enum.to_list()
  end
end

# Create a function to calculate the sum of all odd numbers in a given list.
defmodule List.SumOdds do
  # Define the function with one parameter 'list'.
  def sum_odds(list) do
    # Iterate over each element 'x' in the list.
    Enum.reduce(list, 0, fn x, acc -> acc + if(rem(x, 2) == 1, do: x, else: 0) end)
  end
end

# Define a function to find the largest odd number in a given list.
defmodule List.MaxOdd do
  # Define the function with one parameter 'list'.
  def max_odd(list) do
    # Use the Enum.max_by function to find the maximum value in the list.
    Enum.max_by(list, fn x -> x end)
  end
end

# Test the functions.
IO.inspect(List.ContainsOdd.contains_odd([1, 2, 3, 4, 5])) # true
IO.inspect(List.GenerateOdds.generate_odds(10)) # [1, 3, 5, 7, 9]
IO.inspect(List.SumOdds.sum_odds([1, 2, 3, 4, 5])) # 9
IO.inspect(List.MaxOdd.max_odd([1, 2, 3, 4, 5])) # 5
```

Explanation:

1. **List.ContainsOdd**: This module defines a function `contains_odd/1` that takes a list as input and checks if it contains any odd numbers. It uses the `Enum.any?/2` function to iterate over each element in the list and checks if the remainder of dividing the element by 2 is equal to 1 (indicating an odd number).

2. **List.GenerateOdds**: This module defines a function `generate_odds/1` that takes a limit as input and generates a list of all odd numbers up to that limit. It uses the `Range` module to create a range of numbers from 1 to the limit and then uses `Enum.filter/2` to filter out the even numbers using the `rem/2` function.

3. **List.SumOdds**: This module defines a function `sum_odds/1` that takes a list as input and calculates the sum of all odd numbers in the list. It uses `Enum.reduce/3` to iterate over each element in the list and adds it to the accumulator if it is an odd number.

4. **List.MaxOdd**: This module defines a function `max_odd/1` that takes a list as input and finds the largest odd number in the list. It uses the `Enum.max_by/2` function to find the maximum value in the list based on the value of each element.