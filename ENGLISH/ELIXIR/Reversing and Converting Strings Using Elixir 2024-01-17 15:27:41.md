```elixir
defmodule ComplexElixirCode do
  defp helper_function(input) do
    input
    |> String.upcase()
    |> String.reverse()
  end

  def main(input) do
    input
    |> Enum.map(&helper_function/1)
    |> Enum.join(", ")
  end
end

ComplexElixirCode.main(["hello", "world", "!"])
# Output: "OLLEH, DLROW, !"

# Explanation:
# 1. defp helper_function(input):
#    - This is a private function that takes an input and performs certain operations on it.
#    - It converts the input to uppercase and then reverses it.
#    - The `defp` keyword makes this function private to the ComplexElixirCode module, meaning it can only be called from within the same module.

# 2. def main(input):
#    - This is the main function that will be called when we run the program.
#    - It takes an input, which is expected to be a list of strings.

# 3. input |> Enum.map(&helper_function/1):
#    - This line applies the helper_function to each element of the input list.
#    - The `Enum.map` function iterates through the input list and calls the helper_function on each element, passing it as an argument.

# 4. ... |> Enum.join(", "):
#    - This line joins the results of the helper_function calls into a single string, separated by commas and spaces.
#    - The `Enum.join` function takes a list of strings and joins them together using the specified separator.

# 5. ComplexElixirCode.main(["hello", "world", "!"]):
#    - This line calls the main function with a list of strings as input: ["hello", "world", "!"]
#    - The output of the main function is printed to the console.
```