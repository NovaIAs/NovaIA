```elixir
# Define a function to calculate the factorial of a number
defmodule Factorial do
  def fact(0), do: 1
  def fact(n) when n > 0, do: n * fact(n-1)
end

# Define a function to check if a number is prime
defmodule Prime do
  def is_prime(1), do: false
  def is_prime(n) when n > 1, do: is_prime_rec(n, 2)

  defp is_prime_rec(n, divisor) when divisor * divisor > n, do: true
  defp is_prime_rec(n, divisor) when rem(n, divisor) == 0, do: false
  defp is_prime_rec(n, divisor), do: is_prime_rec(n, divisor+1)
end

# Define a function to calculate the Fibonacci sequence
defmodule Fibonacci do
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n) when n > 1, do: fib(n-1) + fib(n-2)
end

# Define a function to sort a list of numbers
defmodule Sort do
  def sort([]), do: []
  def sort([head | tail]), do: insert(head, sort(tail))

  defp insert(element, []), do: [element]
  defp insert(element, [head | tail]) when element <= head, do: [element | head | tail]
  defp insert(element, [head | tail]), do: [head | insert(element, tail)]
end

# Define a function to reverse a list
defmodule Reverse do
  def reverse([]), do: []
  def reverse([head | tail]), do: reverse(tail) ++ [head]
end

# Define a function to find the maximum value in a list
defmodule Max do
  def max([]), do: nil
  def max([head | tail]), do: max_rec(head, tail)

  defp max_rec(max_so_far, []), do: max_so_far
  defp max_rec(max_so_far, [head | tail]) when head > max_so_far, do: max_rec(head, tail)
  defp max_rec(max_so_far, [_ | tail]), do: max_rec(max_so_far, tail)
end

# Define a function to find the minimum value in a list
defmodule Min do
  def min([]), do: nil
  def min([head | tail]), do: min_rec(head, tail)

  defp min_rec(min_so_far, []), do: min_so_far
  defp min_rec(min_so_far, [head | tail]) when head < min_so_far, do: min_rec(head, tail)
  defp min_rec(min_so_far, [_ | tail]), do: min_rec(min_so_far, tail)
end

# Define a function to find the sum of a list of numbers
defmodule Sum do
  def sum([]), do: 0
  def sum([head | tail]), do: head + sum(tail)
end

# Define a function to find the average of a list of numbers
defmodule Average do
  def average([]), do: nil
  def average([head | tail]), do: sum([head | tail]) / length([head | tail])
end

# Define a function to find the median of a list of numbers
defmodule Median do
  def median([]), do: nil
  def median([head | tail]), do: median_rec(sort([head | tail]))

  defp median_rec([head | tail]) when length([head | tail]) == 1, do: head
  defp median_rec([head | tail]) when rem(length([head | tail]), 2) == 0, do: (head + hd(tl(tail))) / 2
  defp median_rec([_ | tail]), do: median_rec(tail)
end

# Define a function to find the mode of a list of numbers
defmodule Mode do
  def mode([]), do: nil
  def mode([head | tail]), do: mode_rec([head | tail], [], 0, head)

  defp mode_rec([], mode, count, _), do: {mode, count}
  defp mode_rec([head | tail], mode, count, current_mode) when head == current_mode, do: mode_rec(tail, mode, count+1, current_mode)
  defp mode_rec([head | tail], mode, count, current_mode) when head > current_mode, do: mode_rec(tail, head, 1, head)
  defp mode_rec([head | tail], mode, count, current_mode), do: mode_rec(tail, mode, count, current_mode)
end

# Define a function to find the range of a list of numbers
defmodule Range do
  def range([]), do: nil
  def range([head | tail]), do: range_rec(sort([head | tail]))

  defp range_rec([head | tail]), do: hd(tail) - head
end

# Define a function to find the variance of a list of numbers
defmodule Variance do
  def variance([]), do: nil
  def variance([head | tail]), do: variance_rec(sort([head | tail]))

  defp variance_rec([head | tail]) do
    mean = average([head | tail])
    sum_of_squares = Enum.reduce(tail, 0, fn x, acc -> acc + (x - mean) ** 2 end)
    sum_of_squares / (length(tail) - 1)
  end
end

# Define a function to find the standard deviation of a list of numbers
defmodule StandardDeviation do
  def standard_deviation([]), do: nil
  def standard_deviation([head | tail]), do: sqrt(variance([head | tail])))
end
```

Explanation:

This code implements a collection of commonly used mathematical and statistical functions in Elixir. Here's a brief explanation of each function:

1. **Factorial:** Calculates the factorial of a given number.

2. **Prime:** Checks if a given number is prime.

3. **Fibonacci:** Calculates the nth Fibonacci number.

4. **Sort:** Sorts a list of numbers in ascending order.

5. **Reverse:** Reverses a list.

6. **Max:** Finds the maximum value in a list.

7. **Min:** Finds the minimum value in a list.

8. **Sum:** Calculates the sum of all numbers in a list.

9. **Average:** Calculates the average of all numbers in a list.

10. **Median:** Finds the median value in a list.

11. **Mode:** Finds the mode (most occurring value) in a list.

12. **Range:** Calculates the range (difference between the maximum and minimum values) of a list.

13. **Variance:** Calculates the variance of a list of numbers.

14. **StandardDeviation:** Calculates the standard deviation of a list of numbers.

This code provides a comprehensive set of mathematical and statistical functions that can be useful for various applications.