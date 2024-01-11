```elixir
defmodule ComplexElixirCode do
  def main() do
    # Define a function to calculate the factorial of a number
    def factorial(n) do
      if n == 0, do: 1, else: n * factorial(n-1)
    end

    # Define a function to generate a list of prime numbers up to a given limit
    def prime_numbers(limit) do
      Enum.filter(2..limit, &is_prime/1)
    end

    # Define a function to check if a number is prime
    def is_prime(n) do
      Enum.all?(2..:math.sqrt(n), &rem(n, &1) != 0)
    end

    # Calculate the factorial of 10
    IO.puts "Factorial of 10: #{factorial(10)}"

    # Generate a list of prime numbers up to 100
    IO.puts "Prime numbers up to 100: #{prime_numbers(100)}"
  end
end

ComplexElixirCode.main()
```

Explanation:

1. `factorial/1` function: This function calculates the factorial of a given number `n`. It uses a recursive approach, where it multiplies `n` by the factorial of `n-1`. If `n` is 0, it returns 1 (base case).

2. `prime_numbers/1` function: This function generates a list of prime numbers up to a given limit. It uses the `Enum.filter/2` function to filter out non-prime numbers from a range of numbers from 2 to the limit.

3. `is_prime/1` function: This function checks if a given number `n` is prime. It uses the `Enum.all?/2` function to check if the remainder of dividing `n` by all numbers from 2 to the square root of `n` is not equal to 0. If this condition is true for all numbers in the range, then `n` is prime.

4. In the `main()` function, we call the `factorial/1` function with the argument 10 and print the result. We also call the `prime_numbers/1` function with the argument 100 and print the list of prime numbers.

This code demonstrates the use of recursion, list comprehensions, and higher-order functions in Elixir. It also showcases the use of the `IO` module for input and output operations.