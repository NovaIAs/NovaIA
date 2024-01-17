```elixir
defmodule FizzBuzz do
  def game(n) when n < 1, do: []
  def game(n) when n >= 1 do
    [if(rem(n, 3) == 0 and rem(n, 5) == 0, do: "FizzBuzz", else:
      if(rem(n, 3) == 0, do: "Fizz", else:
        if(rem(n, 5) == 0, do: "Buzz", else: n))) | game(n-1)]
  end
end

IO.inspect FizzBuzz.game(15)
```

Explanation:

1. `defmodule FizzBuzz do`: This line defines a module named `FizzBuzz` which will contain the code for the FizzBuzz game.

2. `def game(n) when n < 1, do: []`: This line defines a function named `game` that takes one argument, `n`. It checks if `n` is less than 1. If `n` is less than 1, it returns an empty list `[]`. This is the base case for the recursive function.

3. `def game(n) when n >= 1 do`: This line defines the recursive case for the `game` function. It checks if `n` is greater than or equal to 1. If `n` is greater than or equal to 1, it proceeds to generate the FizzBuzz sequence.

4. `[if(rem(n, 3) == 0 and rem(n, 5) == 0, do: "FizzBuzz", else: ...)]`: This line uses the `if` expression to determine what to display for the current value of `n`.

   - `if(rem(n, 3) == 0 and rem(n, 5) == 0)`: This condition checks if `n` is divisible by both 3 and 5. If it is, it displays "FizzBuzz".

   - `else`: If `n` is not divisible by both 3 and 5, it executes the `else` block.

   - `if(rem(n, 3) == 0)`: This condition checks if `n` is divisible by 3. If it is, it displays "Fizz".

   - `else`: If `n` is not divisible by 3, it executes the `else` block.

   - `if(rem(n, 5) == 0)`: This condition checks if `n` is divisible by 5. If it is, it displays "Buzz".

   - `else`: If `n` is not divisible by 5, it displays the value of `n` itself as a string.

5. `n | game(n-1)`: This line recursively calls the `game` function with `n-1` as the argument. It continues this process until `n` becomes less than 1, at which point the base case is reached and the recursion stops.

6. Finally, the `IO.inspect FizzBuzz.game(15)` line calls the `game` function with the argument `15` and prints the result to the console.