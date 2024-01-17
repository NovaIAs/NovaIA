```elixir
# Define a function to calculate the factorial of a number
def factorial(n) do
  if n == 0, do: 1, else: n * factorial(n - 1)
end

# Define a function to calculate the Fibonacci sequence
def fibonacci(n) do
  if n < 2, do: n, else: fibonacci(n - 1) + fibonacci(n - 2)
end

# Define a function to check if a number is prime
def is_prime?(n) do
  if n < 2, do: false, else: is_prime_helper(n, 2)
end

defp is_prime_helper(n, divisor) do
  if divisor == n, do: true, else:
    if rem(n, divisor) == 0, do: false, else: is_prime_helper(n, divisor + 1)
  end
end

# Define a function to find the greatest common divisor of two numbers
def gcd(a, b) do
  if b == 0, do: a, else: gcd(b, rem(a, b))
end

# Define a function to find the least common multiple of two numbers
def lcm(a, b) do
  a * b / gcd(a, b)
end

# Define a function to generate a random number between 1 and 100
def random_number do
  :rand.uniform(100) + 1
end

# Define a function to generate a random string of a given length
def random_string(length) do
  Enum.map(1..length, fn _ -> random_letter() end) |> Enum.join()
end

defp random_letter do
  :rand.uniform(26) + 65 |> Integer.to_char()
end

# Define a function to sort a list of numbers in ascending order
def sort_numbers(list) do
  list |> Enum.sort()
end

# Define a function to sort a list of strings in lexicographical order
def sort_strings(list) do
  list |> Enum.sort(&(&1 < &2))
end

# Define a function to find the maximum value in a list
def max(list) do
  list |> Enum.max()
end

# Define a function to find the minimum value in a list
def min(list) do
  list |> Enum.min()
end

# Define a function to sum a list of numbers
def sum(list) do
  list |> Enum.reduce(0, &(&1 + &2))
end

# Define a function to calculate the average of a list of numbers
def average(list) do
  sum(list) / length(list)
end

# Define a function to find the median of a list of numbers
def median(list) do
  sorted_list = sort_numbers(list)
  length = length(sorted_list)

  if rem(length, 2) == 1, do: sorted_list |> List.item(length // 2 + 1), else:
    (sorted_list |> List.item(length // 2) + sorted_list |> List.item(length // 2 + 1)) / 2
  end
end

# Define a function to find the mode of a list of numbers
def mode(list) do
  frequencies = list |> Enum.frequencies()
  max_frequency = Enum.max(frequencies)
  Enum.filter(frequencies, fn {_, frequency} -> frequency == max_frequency end) |> Enum.map(fn {number, _} -> number end)
end

# Define a function to find the range of a list of numbers
def range(list) do
  max(list) - min(list)
end

# Define a function to find the variance of a list of numbers
def variance(list) do
  mean = average(list)
  sum_of_squared_differences = list |> Enum.map(fn number -> :math.pow(number - mean, 2) end) |> Enum.sum()
  sum_of_squared_differences / (length(list) - 1)
end

# Define a function to find the standard deviation of a list of numbers
def standard_deviation(list) do
  :math.sqrt(variance(list))
end
```

This code contains a collection of useful mathematical functions in Elixir. It includes functions for calculating the factorial, Fibonacci sequence, checking if a number is prime, finding the greatest common divisor and least common multiple, generating random numbers and strings, sorting lists, finding the maximum, minimum, sum, average, median, mode, range, variance, and standard deviation of a list of numbers. These functions are commonly used in various mathematical and statistical applications and can be easily integrated into Elixir programs.