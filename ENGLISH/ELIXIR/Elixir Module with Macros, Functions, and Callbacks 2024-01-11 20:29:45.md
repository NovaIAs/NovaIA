```elixir
defmodule MyModule do
  defmacro my_macro(arg1, arg2) do
    quote do
      IO.puts "arg1: #{arg1}, arg2: #{arg2}"
    end
  end

  def my_function(arg1, arg2) do
    IO.puts "arg1: #{arg1}, arg2: #{arg2}"
  end

  defp my_private_function(arg1, arg2) do
    IO.puts "arg1: #{arg1}, arg2: #{arg2}"
  end

  def callback_function(arg1, arg2) do
    IO.puts "arg1: #{arg1}, arg2: #{arg2}"
  end
end

# Example of using the macro
MyModule.my_macro("Hello", "World")

# Example of calling a public function
MyModule.my_function("Foo", "Bar")

# Example of using a callback function
MyModule.callback_function(fn(arg1, arg2) ->
  IO.puts "Callback function: #{arg1}, #{arg2}"
end)
```

Explanation:

1. `defmodule MyModule do`: This line defines a new module named `MyModule`.

2. `defmacro my_macro(arg1, arg2) do`: This line defines a macro named `my_macro` that takes two arguments, `arg1` and `arg2`. The `defmacro` keyword is used to define macros in Elixir.

3. `quote do`: The `quote` macro allows you to generate Elixir code at compile time. The code inside the `quote` block is evaluated at compile time and the result is inserted into the generated code.

4. `IO.puts "arg1: #{arg1}, arg2: #{arg2}"`: This line uses the `IO.puts` function to print the values of `arg1` and `arg2` to the console.

5. `end`: This line ends the definition of the `my_macro` macro.

6. `def my_function(arg1, arg2) do`: This line defines a public function named `my_function` that takes two arguments, `arg1` and `arg2`. The `def` keyword is used to define functions in Elixir.

7. `IO.puts "arg1: #{arg1}, arg2: #{arg2}"`: This line uses the `IO.puts` function to print the values of `arg1` and `arg2` to the console.

8. `end`: This line ends the definition of the `my_function` function.

9. `defp my_private_function(arg1, arg2) do`: This line defines a private function named `my_private_function` that takes two arguments, `arg1` and `arg2`. The `defp` keyword is used to define private functions in Elixir.

10. `IO.puts "arg1: #{arg1}, arg2: #{arg2}"`: This line uses the `IO.puts` function to print the values of `arg1` and `arg2` to the console.

11. `end`: This line ends the definition of the `my_private_function` function.

12. `def callback_function(arg1, arg2) do`: This line defines a public function named `callback_function` that takes two arguments, `arg1` and `arg2`. The `def` keyword is used to define functions in Elixir.

13. `IO.puts "arg1: #{arg1}, arg2: #{arg2}"`: This line uses the `IO.puts` function to print the values of `arg1` and `arg2` to the console.

14. `end`: This line ends the definition of the `callback_function` function.

15. `MyModule.my_macro("Hello", "World")`: This line calls the `my_macro` macro with the arguments `"Hello"` and `"World"`.

16. `MyModule.my_function("Foo", "Bar")`: This line calls the `my_function` function with the arguments `"Foo"` and `"Bar"`.

17. `MyModule.callback_function(fn(arg1, arg2) ->
  IO.puts "Callback function: #{arg1}, #{arg2}"
end)`: This line calls the `callback_function` function with a callback function as the first argument. The callback function takes two arguments, `arg1` and `arg2`, and prints the values of `arg1` and `arg2` to the console.