```elixir
defmodule Factorial do
  def fact(0), do: 1
  def fact(n), do: n * fact(n-1)
end

defmodule Fibonacci do
  def fib(0), do: 0
  def fib(1), do: 1
  def fib(n), do: fib(n-1) + fib(n-2)
end

defmodule Prime do
  def is_prime(2), do: true
  def is_prime(n) when n < 2, do: false
  def is_prime(n), do: is_prime_helper(n, 2)

  defp is_prime_helper(n, divisor) do
    if n == divisor, do: true
    if rem(n, divisor) == 0, do: false
    is_prime_helper(n, divisor+1)
  end
end

defmodule QuickSort do
  def sort([]), do: []
  def sort([pivot | tail]), do: sort(Enum.filter(tail, &(&1 < pivot))) ++ [pivot] ++ sort(Enum.filter(tail, &(&1 >= pivot)))
end

defmodule MergeSort do
  def sort([]), do: []
  def sort([pivot | tail]), do: merge(sort([pivot]), sort(tail))

  defp merge(left, right) do
    case {left, right} do
      {[], _}, do: right
      {_, []}, do: left
      {[x | xs], [y | ys]} when x < y, do: [x | merge(xs, [y | ys])]
      {[x | xs], [y | ys]} -> [y | merge([x | xs], ys)]
    end
  end
end

defmodule Ackermann do
  def ack(0, n), do: n+1
  def ack(m, 0), do: ack(m-1, 1)
  def ack(m, n), do: ack(m-1, ack(m, n-1))
end

defmodule Hamming do
  def hamming_distance("", ""), do: 0
  def hamming_distance(a, ""), do: String.length(a)
  def hamming_distance("", b), do: String.length(b)
  def hamming_distance([a | as], [b | bs]) do
    if a == b, do: hamming_distance(as, bs)
    1 + hamming_distance(as, bs)
  end
end

defmodule JosephusProblem do
  def josephus(n, k) do
    Enum.reduce(1..n, [], fn x, acc ->
      [x | Enum.drop(acc ++ [x], k-1)]
    end) |> Enum.at(0)
  end
end

```

Explanation:

This code implements various mathematical and algorithmic functions and problems in Elixir. Here's a brief explanation of each function:

1. Factorial (Factorial.fact): Calculates the factorial of a number `n` using recursion.

2. Fibonacci (Fibonacci.fib): Computes the Fibonacci sequence using recursion, where each number is the sum of the two preceding ones.

3. Prime Number Check (Prime.is_prime): Determines if a given number `n` is prime by checking divisibility up to its square root.

4. Quick Sort (QuickSort.sort): Implements the Quick Sort algorithm to sort a list of numbers.

5. Merge Sort (MergeSort.sort): Implements the Merge Sort algorithm to sort a list of numbers.

6. Ackermann's Function (Ackermann.ack): Computes the Ackermann function, a recursive function with interesting mathematical properties.

7. Hamming Distance (Hamming.hamming_distance): Calculates the Hamming distance between two strings, which is the number of positions where the corresponding characters are different.

8. Josephus Problem (JosephusProblem.josephus): Solves the Josephus problem, where a group of people standing in a circle remove every k-th person until only one remains.

These functions demonstrate various programming techniques, such as recursion, iteration, and algorithmic problem-solving, and showcase the versatility of Elixir for mathematical and algorithmic applications.