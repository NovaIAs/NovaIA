```elixir
defmodule MyModule do
  @moduledoc """
  This module does something very complex and differentiated.
  """

  defmacro my_macro(do: expr) do
    IO.inspect expr
  end

  def my_function(arg1, arg2, arg3 \\ nil) do
    if arg3 != nil do
      IO.inspect arg3
    end

    arg1 + arg2
  end

  def my_callback(value, reason) do
    IO.inspect value
    IO.inspect reason
  end

  defstruct [:field1, :field2, :field3]
end

defimpl GenServer, for: MyModule do
  def init(_args) do
    {:ok, nil}
  end

  def handle_call(:ping, _from, state) do
    {:reply, :pong, state}
  end

  def handle_cast({:stop, reason}, state) do
    {:stop, :normal, reason, state}
  end
end

# Usage

MyModule.my_macro do
  "Hello, world!"
end

IO.inspect MyModule.my_function(1, 2)
IO.inspect MyModule.my_function(1, 2, 3)

MyModule.start_link()
GenServer.cast(MyModule, {:stop, :normal})

MyStruct = MyModule.defstruct
MyStruct{field1: "value1", field2: "value2", field3: "value3"}
```

Explanation:

1. `defmodule MyModule do`: This line defines a module named `MyModule`.


2. `@moduledoc """": This line is a module documentation string. It provides a description of what the module does.


3. `defmacro my_macro(do: expr)`: This line defines a macro named `my_macro`. Macros are used to write custom syntax in Elixir. In this case, the `my_macro` macro takes an expression `expr` as its argument and prints it using `IO.inspect`.


4. `def my_function(arg1, arg2, arg3 \\ nil)`: This line defines a function named `my_function`. It takes three arguments, `arg1`, `arg2`, and `arg3`, where `arg3` has a default value of `nil`. The function adds `arg1` and `arg2` and prints `arg3` if it's not `nil`.


5. `def my_callback(value, reason)`: This line defines a callback function named `my_callback`. It takes two arguments, `value` and `reason`, and prints them using `IO.inspect`.


6. `defstruct [:field1, :field2, :field3]`: This line defines a struct named `MyStruct` with three fields: `field1`, `field2`, and `field3`.


7. `defimpl GenServer, for: MyModule do`: This line defines an implementation of the `GenServer` behavior for the `MyModule` module.


8. `def init(_args)`: This function is called when the `GenServer` process is started. It initializes the process state to `nil`.


9. `def handle_call(:ping, _from, state)`: This function handles `:ping` messages sent to the `GenServer` process. It returns `{:reply, :pong, state}` to reply with `:pong` and maintain the current state.


10. `def handle_cast({:stop, reason}, state)`: This function handles `{:stop, reason}` messages sent to the `GenServer` process. It stops the process with `:normal` reason and the current state.


11. `MyModule.my_macro do`: This line calls the `my_macro` macro with an expression that prints "Hello, world!" to the console.


12. `IO.inspect MyModule.my_function(1, 2)`: This line calls the `my_function` function with arguments 1 and 2 and prints its return value to the console.


13. `IO.inspect MyModule.my_function(1, 2, 3)`: This line calls the `my_function` function with arguments 1, 2, and 3 and prints its return value to the console.


14. `MyModule.start_link()`: This line starts the `GenServer` process for the `MyModule` module.


15. `GenServer.cast(MyModule, {:stop, :normal})`: This line sends a `{:stop, :normal}` message to the `GenServer` process, causing it to stop gracefully.


16. `MyStruct = MyModule.defstruct`: This line assigns the `MyStruct` struct to a variable named `MyStruct`.


17. `MyStruct{field1: "value1", field2: "value2", field3: "value3"}`: This line creates a new `MyStruct` instance with the specified field values and assigns it to a variable.