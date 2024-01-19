```elixir
defmodule FunctionName do
  def my_function(arg1, arg2, arg3 \\ nil) do
    cond do
      is_nil(arg3) ->
        # Do something with arg1 and arg2
      true ->
        # Do something with arg1, arg2, and arg3
    end
  end
end

Enum.each([1, 2, 3], &FunctionName.my_function(&1, &2))
FunctionName.my_function(1, 2, 3)
```

Explanation:

1. **Module Definition**: We start by defining a module named `FunctionName`. Modules in Elixir are used for organizing and structuring code.

2. **Function Definition**: Inside the module, we define a function called `my_function` with three parameters: `arg1`, `arg2`, and an optional parameter `arg3` with a default value of `nil`.

3. **Conditional Branching with `cond`**: We use the `cond` special form to perform conditional branching. The `cond` statement allows us to specify multiple conditions and the corresponding actions to be taken when each condition is met.

4. **Using `is_nil` to Check for `nil`**: We use the `is_nil/1` function to check if `arg3` is `nil`. If `arg3` is `nil`, we execute the first branch of the `cond` statement, which performs some action with `arg1` and `arg2`.

5. **Else Clause**: If `arg3` is not `nil`, we execute the `else` clause of the `cond` statement. In this case, we perform some action with `arg1`, `arg2`, and `arg3`.

6. **Calling the Function**: We demonstrate how to call the `my_function` function in two different ways:

   - `Enum.each([1, 2, 3], &FunctionName.my_function(&1, &2))`: This line uses the `Enum.each/3` function to iterate over the list `[1, 2, 3]`. For each element in the list, it calls the `my_function` function with the element itself (`&1`) and the next element in the list (`&2`) as arguments. This demonstrates how we can pass arguments to a function using anonymous functions.

   - `FunctionName.my_function(1, 2, 3)`: This line directly calls the `my_function` function with the arguments `1`, `2`, and `3`. This showcases how we can explicitly specify the arguments to the function.

In summary, this code demonstrates how to define a function with optional parameters, use conditional branching with `cond`, and call the function in different ways in Elixir.