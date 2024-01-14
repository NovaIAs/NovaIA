```elixir
defmodule ComplexElixirCode do
  defmodule ComplexStruct do
    defstruct [:field1, :field2, :field3]
  end

  defmodule ComplexFunction do
    def complex_function(arg1, arg2, arg3) do
      case arg1 do
        :option1 -> {:ok, arg2}
        :option2 -> {:error, arg3}
      end
    end
  end

  defmodule ComplexListModule do
    defmodule ComplexNestedModule do
      def complex_nested_function(arg1, arg2) do
        arg1 + arg2
      end
    end

    def complex_list_function(arg1, arg2) do
      Enum.map(arg1, fn x -> ComplexNestedModule.complex_nested_function(x, arg2) end)
    end
  end

  defmodule ComplexMapModule do
    def complex_map_function(arg1, arg2) do
      Map.new(arg1, fn {k, v} -> {k, v + arg2} end)
    end
  end

  defmodule ComplexProcessModule do
    def start_complex_process(arg1, arg2) do
      spawn(fn -> complex_process_function(arg1, arg2) end)
    end

    defp complex_process_function(arg1, arg2) do
      receive do
        {:message, msg} ->
          IO.puts("Received message: #{msg}")
          complex_process_function(arg1, arg2)
        {:stop} ->
          IO.puts("Process stopped.")
      end
    end
  end
end
```

Explanation:

1. `ComplexStruct`: This module defines a complex Elixir struct with three fields: `field1`, `field2`, and `field3`. Structs are used to represent data in Elixir.

2. `ComplexFunction`: This module defines a complex function called `complex_function` that takes three arguments and returns a tuple containing either an `:ok` or `:error` atom and a value. The function's behavior depends on the value of the first argument.

3. `ComplexListModule`: This module defines a complex Elixir list module. It contains a nested module called `ComplexNestedModule` and a function called `complex_list_function`. The nested module defines a function called `complex_nested_function`, which takes two arguments and returns their sum. The `complex_list_function` function takes two arguments: a list and a value. It applies the `complex_nested_function` function to each element of the list and returns a new list with the results.

4. `ComplexMapModule`: This module defines a complex Elixir map module. It contains a function called `complex_map_function` that takes two arguments: a map and a value. The function creates a new map by iterating over the key-value pairs in the original map and adding the specified value to each value.

5. `ComplexProcessModule`: This module defines a complex Elixir process module. It contains a function called `start_complex_process` that takes two arguments and returns a process ID. The function spawns a new process by calling the `spawn` function with an anonymous function. The anonymous function defines a `complex_process_function` function that receives messages via pattern matching and prints them to the console. The process continues running until it receives a message with the atom `{:stop}`.