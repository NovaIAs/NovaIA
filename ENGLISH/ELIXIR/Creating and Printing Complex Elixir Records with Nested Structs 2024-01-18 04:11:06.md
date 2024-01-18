```elixir
defmodule ComplexElixirCode do
  defmodule Record do
    defstruct [:name, :age, :address]
  end

  defmodule Address do
    defstruct [:street, :city, :state, :zip]
  end

  def create_record(name, age, street, city, state, zip) do
    %Record{
      name: name,
      age: age,
      address: %Address{
        street: street,
        city: city,
        state: state,
        zip: zip
      }
    }
  end

  def print_record(record) do
    IO.puts "Name: #{record.name}"
    IO.puts "Age: #{record.age}"
    IO.puts "Address:"
    IO.puts "\tStreet: #{record.address.street}"
    IO.puts "\tCity: #{record.address.city}"
    IO.puts "\tState: #{record.address.state}"
    IO.puts "\tZip: #{record.address.zip}"
  end

  def main do
    record = create_record(
      "John Doe",
      30,
      "123 Main Street",
      "Anytown",
      "CA",
      "12345"
    )

    print_record(record)
  end
end

ComplexElixirCode.main()
```

Explanation:

1. **Creating Custom Modules and Records**:
   - We define two modules: `Record` and `Address`.
   - `Record` represents a person with fields like name, age, and `Address`.
   - `Address` represents an address with fields like street, city, state, and zip.

2. **Creating a Record**:
   - We use the `create_record` function to create a new `Record` struct.
   - We pass the person's name, age, and address information as arguments.

3. **Printing the Record**:
   - The `print_record` function prints the details of the given record.
   - It prints the person's name, age, and address information in a structured format.

4. **Main Function**:
   - The `main` function is the entry point of the program.
   - It calls `create_record` to create a new record and then calls `print_record` to display the record details.

5. **Running the Program**:
   - To run the program, compile and execute the Elixir script using `elixir ComplexElixirCode.exs`.

This program demonstrates how to create custom structs (records) and modules in Elixir. It showcases the use of nested structs and how to pass data between functions.