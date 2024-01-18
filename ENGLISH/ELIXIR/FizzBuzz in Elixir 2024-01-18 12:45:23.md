```elixir
defmodule FizzBuzz do
  def get_integer_range(start, stop) do
    Enum.to_list(start..stop)
  end

  def get_fizz_buzz(number) do
    case rem(number, 3) do
      0 -> "Fizz"
      _ -> case rem(number, 5) do
              0 -> "Buzz"
              _ -> number
            end
    end
  end

  def print_fizz_buzz(start, stop) do
    integer_range = get_integer_range(start, stop)
    Enum.each(integer_range, fn number -> IO.puts(get_fizz_buzz(number)) end)
  end
end

FizzBuzz.print_fizz_buzz(1, 100)
```

Explanation:

1. The `FizzBuzz` module is defined as a namespace for the functions and data structures related to the FizzBuzz program.

2. The `get_integer_range/2` function takes two integer arguments, `start` and `stop`, and returns a list of integers in the range from `start` to `stop`, inclusive.

3. The `get_fizz_buzz/1` function takes an integer argument, `number`, and returns a string representing the FizzBuzz value for that number. If the number is divisible by 3, it returns "Fizz". If the number is divisible by 5, it returns "Buzz". If the number is divisible by both 3 and 5, it returns "FizzBuzz". Otherwise, it returns the number as a string.

4. The `print_fizz_buzz/2` function takes two integer arguments, `start` and `stop`, and prints the FizzBuzz values for the numbers in the range from `start` to `stop`, inclusive, to the standard output.

5. The `FizzBuzz.print_fizz_buzz(1, 100)` expression calls the `print_fizz_buzz/2` function with the arguments 1 and 100, which prints the FizzBuzz values for the numbers from 1 to 100.

This program is a classic programming exercise that tests a programmer's understanding of basic programming concepts such as loops, conditional statements, and functions. It is also a good example of how Elixir can be used to write concise and readable code.