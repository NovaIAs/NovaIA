```elixir
defmodule ComplexElixirCode do
  # Simulate a complex business logic with a function that receives a list of numbers and returns a tuple with the sum, mean, and median of the numbers.
  def calculate_stats(numbers) do
    sum = Enum.reduce(numbers, 0, fn n, acc -> n + acc end)
    mean = sum / length(numbers)

    # Sort the list to calculate the median
    sorted_numbers = Enum.sort(numbers)

    # Calculate the median based on the length of the sorted list
    median =
      case length(sorted_numbers) do
        1 -> List.first(sorted_numbers)
        even ->
          mid_index = div(length(sorted_numbers), 2)
          (sorted_numbers[mid_index - 1] + sorted_numbers[mid_index]) / 2
        _ -> sorted_numbers[div(length(sorted_numbers), 2)]
      end

    # Return a tuple with the calculated values
    {sum, mean, median}
  end

  # Define a function to generate a list of random numbers
  def generate_random_numbers(count) do
    for _ <- 1..count, do: :rand.uniform(100)
  end

  # Simulate a complex data structure with a map that contains different types of values
  def complex_data_structure do
    %{
      :name => "John Doe",
      :age => 30,
      :address => %{
        :street => "123 Main Street",
        :city => "Anytown",
        :state => "CA",
        :zip => "12345"
      },
      :hobbies => ["reading", "hiking", "programming"],
      :friends => [
        %{:name => "Jane Doe", :age => 25},
        %{:name => "John Smith", :age => 35}
      ]
    }
  end

  # Simulate a recursive function that calculates the factorial of a number
  def factorial(n) do
    if n <= 1, do: 1, else: n * factorial(n - 1)
  end

  # Simulate a complex pattern matching case with a function that receives a list of values and returns the sum of the even numbers and the product of the odd numbers.
  def calculate_even_odd_sum_product(numbers) do
    Enum.reduce(numbers, {0, 1}, fn n, {even_sum, odd_product} ->
      case rem(n, 2) do
        0 -> {even_sum + n, odd_product}
        _ -> {even_sum, odd_product * n}
      end
    end)
  end
end
```

Explanation:

1. The `ComplexElixirCode` module is defined, which contains various complex functions and data structures.

2. The `calculate_stats` function takes a list of numbers and calculates the sum, mean, and median of those numbers. It uses `Enum.reduce` to calculate the sum, divides the sum by the length of the list to get the mean, and sorts the list before calculating the median based on the list's length.

3. The `generate_random_numbers` function generates a list of random numbers of a specified count.

4. The `complex_data_structure` function defines a complex data structure as a map containing different types of values, including strings, integers, maps, and lists.

5. The `factorial` function calculates the factorial of a given number using recursion. It multiplies the number by the factorial of the previous number until it reaches 1.

6. The `calculate_even_odd_sum_product` function takes a list of numbers and calculates the sum of the even numbers and the product of the odd numbers using `Enum.reduce`. It branches based on the remainder when dividing the number by 2 to determine if it's even or odd.

This code demonstrates various complex features of Elixir, including pattern matching, recursion, and the use of modules and functions. It showcases the expressive nature of Elixir in solving complex problems and handling various data structures.