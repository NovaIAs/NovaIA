```elixir
defmodule Fibonacci do
  @doc """
  Calculates the nth Fibonacci number.

  The Fibonacci sequence is defined as follows:
  F0 = 0
  F1 = 1
  Fn = Fn-1 + Fn-2 for n > 1

  ## Examples

      iex> Fibonacci.calculate(0)
      0
      iex> Fibonacci.calculate(1)
      1
      iex> Fibonacci.calculate(10)
      55
  """
  def calculate(n) when n < 0, do: raise ArgumentError, message: "n must be non-negative"

  def calculate(0), do: 0
  def calculate(1), do: 1

  def calculate(n) do
    calculate(n - 1) + calculate(n - 2)
  end
end

defmodule Factorial do
  @doc """
  Calculates the factorial of a non-negative integer.

  The factorial of a non-negative integer n is defined as the product of all positive integers less than or equal to n.

  ## Examples

      iex> Factorial.calculate(0)
      1
      iex> Factorial.calculate(5)
      120
  """
  def calculate(n) when n < 0, do: raise ArgumentError, message: "n must be non-negative"

  def calculate(0), do: 1

  def calculate(n) do
    n * calculate(n - 1)
  end
end

defmodule Prime do
  @doc """
  Checks if a given number is prime.

  A prime number is a natural number greater than 1 that has no positive divisors other than 1 and itself.

  ## Examples

      iex> Prime.is_prime?(2)
      true
      iex> Prime.is_prime?(10)
      false
  """
  def is_prime?(n) when n < 2, do: false

  def is_prime?(2), do: true

  def is_prime?(n) do
    Enum.all?(2..(div(n, 2)), fn x -> rem(n, x) != 0 end)
  end
end

defmodule StringUtilities do
  @doc """
  Reverses a string.

  ## Examples

      iex> StringUtilities.reverse("Hello")
      "olleH"
  """
  def reverse(string) do
    string |> String.graphemes() |> Enum.reverse() |> Enum.join()
  end

  @doc """
  Capitalizes the first letter of a string.

  ## Examples

      iex> StringUtilities.capitalize("hello")
      "Hello"
  """
  def capitalize(string) do
    string |> String.graphemes() |> Enum.map(&String.capitalize/1) |> Enum.join()
  end

  @doc """
  Checks if a string is a palindrome.

  A palindrome is a word, phrase, or sequence that reads the same backwards and forwards.

  ## Examples

      iex> StringUtilities.is_palindrome?("racecar")
      true
      iex> StringUtilities.is_palindrome?("hello")
      false
  """
  def is_palindrome?(string) do
    string = string |> String.downcase() |> String.replace(~r/[^a-z0-9]/, "")
    string == String.reverse(string)
  end
end

defmodule ListUtilities do
  @doc """
  Finds the maximum value in a list.

  ## Examples

      iex> ListUtilities.max([1, 2, 3, 4, 5])
      5
  """
  def max(list) do
    Enum.reduce(list, fn x, acc -> max(x, acc) end)
  end

  @doc """
  Finds the minimum value in a list.

  ## Examples

      iex> ListUtilities.min([1, 2, 3, 4, 5])
      1
  """
  def min(list) do
    Enum.reduce(list, fn x, acc -> min(x, acc) end)
  end

  @doc """
  Checks if a list contains a given element.

  ## Examples

      iex> ListUtilities.contains?([1, 2, 3, 4, 5], 3)
      true
      iex> ListUtilities.contains?([1, 2, 3, 4, 5], 6)
      false
  """
  def contains?(list, element) do
    Enum.member?(list, element)
  end

  @doc """
  Removes all occurrences of a given element from a list.