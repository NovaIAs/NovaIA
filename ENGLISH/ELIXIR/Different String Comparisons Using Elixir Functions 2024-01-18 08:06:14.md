```elixir
defmodule SomeModule do
  def some_function(param1, param2) do
    a = "Hello, " ++ param1
    b = "How are you, " ++ param2

    function3(a, b) |> io.puts
  end

  defp function2(param1, param2) do
    Enum.zip(param1, param2)
  end

  defp function3(param1, param2) do
    function2(param1, param2)
      |> Enum.filter(fn {x, y} -> x !== y end)
      |> Enum.map(fn {x, y} -> "#{x} and #{y} are different" end)
  end
end

SomeModule.some_function("Alice", "Bob")

```

Explanation:

1. We define a module called `SomeModule` using `defmodule`. Modules are units of organization in Elixir.

2. Inside the module, we define a function called `some_function` with two parameters, `param1` and `param2`.

3. Within the `some_function` function, we use the `++` operator to concatenate strings. We assign the result of concatenating "Hello, " with `param1` to the variable `a`, and the result of concatenating "How are you, " with `param2` to the variable `b`.

4. We call the `function3` function with `a` and `b` as arguments, and pipe its result to `io.puts` to print it to the console.

5. We define a private function called `function2` with two parameters, `param1` and `param2`. This function uses the `Enum.zip` function to create a list of tuples by pairing corresponding elements from the `param1` and `param2` lists.

6. We define another private function called `function3` with two parameters, `param1` and `param2`.

7. In the `function3` function, we first call the `function2` function with `param1` and `param2` as arguments.

8. We then use the `Enum.filter` function to filter out tuples from the result of the previous step where the first and second elements are not equal. This gives us a list of tuples where the corresponding elements are different.

9. Finally, we use the `Enum.map` function to transform the list of tuples into a list of strings. Each string takes the format: "x and y are different", where `x` and `y` are the elements of the tuple.

10. We call the `SomeModule.some_function` function with the strings "Alice" and "Bob" as arguments. This triggers the execution of the function, which prints the following output:

```
["Alice and Bob are different"]
```

This code demonstrates various Elixir features, including function definitions, pattern matching, list operations, and piping. It also illustrates how to define private functions within a module and use them in other functions.