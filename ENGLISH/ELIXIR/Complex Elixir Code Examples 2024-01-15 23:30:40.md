```elixir
defmodule ComplexElixirCode do
  defmacro my_macro(expr) do
    quote do
      IO.puts "This is my macro: #{expr}"
    end
  end

  defmodule NestedModule do
    def my_nested_function(arg) do
      IO.puts "This is my nested function: #{arg}"
    end
  end

  def my_function(arg) do
    result = NestedModule.my_nested_function(arg)
    IO.puts "This is my function: #{result}"
  end

  def my_list_comprehension do
    [x * 2 || x <- [1, 2, 3]]
  end

  def my_pipe_operator do
    [1, 2, 3]
    |> Enum.map(&(&1 * 2))
    |> Enum.filter(&(&1 > 4))
  end

  def my_keyword_list do
    %{
      :name => "John Doe",
      :age => 30,
      :city => "New York"
    }
  end

  def my_map do
    Map.put(%{}, :name, "John Doe")
  end

  def my_tuple do
    {:ok, "Hello, world!"}
  end

  def my_process do
    spawn(fn -> IO.puts "This is a process" end)
  end
end
```

Explanation:

1. `defmacro my_macro(expr)`: This defines a macro called `my_macro` that takes an expression `expr` as an argument. Macros are used to extend the Elixir language with new syntax or functionality.

2. `quote do ... end`: This is the body of the macro. It uses the `quote` macro to generate Elixir code dynamically. The generated code will be inserted in place of the macro call.

3. `IO.puts "This is my macro: #{expr}"`: This line uses the `IO.puts` function to print a message to the console. The `#{expr}` part is interpolated into the string, so the value of the `expr` argument will be printed.

4. `defmodule NestedModule do ... end`: This defines a nested module called `NestedModule` inside the `ComplexElixirCode` module. Nested modules are used to organize code into logical groups and can be accessed using the `.` operator.

5. `def my_nested_function(arg)`: This defines a function called `my_nested_function` inside the `NestedModule` module. It takes an argument `arg` and prints a message to the console.

6. `def my_function(arg)`: This defines a function called `my_function` in the `ComplexElixirCode` module. It calls the `my_nested_function` function from the nested module and prints a message to the console.

7. `def my_list_comprehension do ... end`: This defines a list comprehension, which is a concise way of creating a list in Elixir. The `[x * 2 || x <- [1, 2, 3]]` part generates a list by multiplying each element in the `[1, 2, 3]` list by 2.

8. `def my_pipe_operator do ... end`: This defines a function that uses the pipe operator (`|>`) to chain together a series of function calls. The `[1, 2, 3]` list is passed through the `Enum.map` function, which multiplies each element by 2. The result is then passed through the `Enum.filter` function, which filters out elements that are greater than 4.

9. `def my_keyword_list do ... end`: This defines a keyword list, which is a collection of key-value pairs in Elixir. The `%{ ... }` syntax is used to create a keyword list.

10. `def my_map do ... end`: This defines a function that creates a map, which is a collection of key-value pairs in Elixir. The `Map.put` function is used to add a new key-value pair to the map.

11. `def my_tuple do ... end`: This defines a function that creates a tuple, which is an ordered collection of values in Elixir. The `{:ok, "Hello, world!"}` tuple contains two elements: `"ok"` and `"Hello, world!"`.

12. `def my_process do ... end`: This defines a function that creates a new process in Elixir. The `spawn` function is used to create a new process, and the anonymous function `fn -> IO.puts "This is a process" end` is passed as the argument. The process will execute the code in the anonymous function.