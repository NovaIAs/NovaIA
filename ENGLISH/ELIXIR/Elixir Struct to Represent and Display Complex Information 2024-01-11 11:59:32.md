```elixir
defmodule ComplexElixirCode do
  defstruct [
    :name,
    :age,
    :occupation,
    :hobbies
  ]

  def new(name, age, occupation, hobbies) do
    %ComplexElixirCode{
      name: name,
      age: age,
      occupation: occupation,
      hobbies: hobbies
    }
  end

  def display_info(%ComplexElixirCode{name: name, age: age, occupation: occupation, hobbies: hobbies}) do
    IO.puts("Name: #{name}")
    IO.puts("Age: #{age}")
    IO.puts("Occupation: #{occupation}")
    IO.puts("Hobbies:")
    Enum.each(hobbies, &IO.puts/1)
  end
end

person = ComplexElixirCode.new("John Doe", 30, "Software Engineer", ["Coding", "Hiking", "Reading"])

ComplexElixirCode.display_info(person)
```

This code defines a complex Elixir struct called `ComplexElixirCode` that can be used to represent a person with their name, age, occupation, and hobbies. The struct is defined using the `defstruct` macro, which generates a constructor function and accessor functions for each field in the struct.

The `new` function is used to create a new `ComplexElixirCode` struct, and the `display_info` function is used to display the information stored in a `ComplexElixirCode` struct.

The `Enum.each` function is used to iterate over the list of hobbies and print each hobby on a separate line.

The `IO.puts` function is used to print output to the console.

This code demonstrates the use of Elixir structs and pattern matching to create a complex data structure and manipulate its fields.