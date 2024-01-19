```elixir
defmodule MyComplexModule do
  # Public API

  def greet(name) do
    "Hello, #{name}!"
  end

  def sum(a, b) do
    a + b
  end

  # Private Helpers

  defp format_name(name) do
    name
    |> String.trim()
    |> String.capitalize()
  end

  defp validate_input(a, b) do
    if is_number(a) and is_number(b) do
      {:ok, a, b}
    else
      {:error, "Inputs must be numbers"}
    end
  end

  # Nested Modules

  defmodule NestedModule do
    def hello() do
      "Hello from the nested module!"
    end
  end
end
```

**Explanation:**

* The `MyComplexModule` module defines two public functions, `greet/1` and `sum/2`, and two private helper functions, `format_name/1` and `validate_input/2`.
* The `greet/1` function takes a name as an argument and returns a greeting message.
* The `sum/2` function takes two numbers as arguments and returns their sum.
* The `format_name/1` function takes a name as an argument and returns a formatted version of the name, with the first letter capitalized and any leading or trailing whitespace removed.
* The `validate_input/2` function takes two values as arguments and verifies that they are both numbers. If they are, it returns a tuple containing the two numbers. If they are not, it returns a tuple containing an error message.
* The `NestedModule` module is a nested module within the `MyComplexModule` module. It defines a single function, `hello/0`, which returns a greeting message.