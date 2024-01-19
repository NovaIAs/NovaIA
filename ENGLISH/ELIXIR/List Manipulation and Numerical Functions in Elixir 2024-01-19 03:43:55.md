```elixir
# Define a function that takes a list of numbers and returns the sum of the even numbers
def sum_even(numbers) do
  Enum.reduce(numbers, 0, fn n, acc ->
    if rem(n, 2) == 0 do
      acc + n
    else
      acc
    end
  end)
end

# Define a function that takes a list of strings and returns the longest string
def longest_string(strings) do
  Enum.reduce(strings, "", fn s, acc ->
    if String.length(s) > String.length(acc) do
      s
    else
      acc
    end
  end)
end

# Define a function that takes a list of maps and returns the map with the highest value for the "value" key
def max_value_map(maps) do
  Enum.reduce(maps, %{}, fn m, acc ->
    if m["value"] > acc["value"] do
      m
    else
      acc
    end
  end)
end

# Define a function that takes a list of tuples and returns a list of the first elements of each tuple
def first_elements(tuples) do
  Enum.map(tuples, fn {a, _} -> a end)
end

# Define a function that takes a list of lists of numbers and returns a single list of all the numbers
def flatten_list(lists) do
  Enum.flat_map(lists, fn l -> l end)
end

# Define a function that takes a list of numbers and returns a tuple containing the minimum and maximum values
def min_max(numbers) do
  {Enum.min(numbers), Enum.max(numbers)}
end

# Define a function that takes a list of numbers and returns the average value
def average(numbers) do
  sum = Enum.reduce(numbers, 0, &+/2)
  sum / length(numbers)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are greater than the average
def greater_than_average(numbers) do
  avg = average(numbers)
  Enum.filter(numbers, fn n -> n > avg end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are less than or equal to the average
def less_than_or_equal_to_average(numbers) do
  avg = average(numbers)
  Enum.filter(numbers, fn n -> n <= avg end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are within a specified range
def within_range(numbers, min, max) do
  Enum.filter(numbers, fn n -> n >= min and n <= max end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are not within a specified range
def not_within_range(numbers, min, max) do
  Enum.filter(numbers, fn n -> n < min or n > max end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are unique
def unique_numbers(numbers) do
  Enum.uniq(numbers)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are repeated
def repeated_numbers(numbers) do
  Enum.filter(numbers, fn n -> Enum.count(numbers, n) > 1 end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are even and greater than 5
def even_and_greater_than_5(numbers) do
  Enum.filter(numbers, fn n -> rem(n, 2) == 0 and n > 5 end)
end

# Define a function that takes a list of numbers and returns a list of the numbers that are odd and less than 10
def odd_and_less_than_10(numbers) do
  Enum.filter(numbers, fn n -> rem(n, 2) != 0 and n < 10 end)
end
```