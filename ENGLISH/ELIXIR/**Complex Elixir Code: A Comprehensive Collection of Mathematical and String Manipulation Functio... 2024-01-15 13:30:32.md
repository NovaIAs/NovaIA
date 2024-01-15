```elixir
defmodule ComplexElixirCode do
  def greet(name) do
    IO.puts "Hello, #{name}!"
  end

  def sum(a, b) do
    a + b
  end

  def factorial(n) do
    cond do
      n == 0 -> 1
      n > 0 -> n * factorial(n-1)
    end
  end

  def fibonacci(n) do
    cond do
      n == 0 -> 0
      n == 1 -> 1
      n > 1 -> fibonacci(n-1) + fibonacci(n-2)
    end
  end

  defmodule Point do
    defstruct [:x, :y]
  end

  def distance_between(point1, point2) do
    {:math.sqrt, :math.pow(point1.x - point2.x, 2) + :math.pow(point1.y - point2.y, 2)}
  end

  defmodule Circle do
    defstruct [:center, :radius]
  end

  def area_of_circle(circle) do
    :math.pi * :math.pow(circle.radius, 2)
  end

  defmodule Rectangle do
    defstruct [:width, :height]
  end

  def area_of_rectangle(rectangle) do
    rectangle.width * rectangle.height
  end

  defmodule Triangle do
    defstruct [:base, :height]
  end

  def area_of_triangle(triangle) do
    0.5 * triangle.base * triangle.height
  end

  defmodule ListUtils do
    def map(list, function) do
      Enum.map(list, function)
    end

    def filter(list, function) do
      Enum.filter(list, function)
    end

    def reduce(list, initial_value, function) do
      Enum.reduce(list, initial_value, function)
    end
  end

  defmodule StringUtils do
    def capitalize(string) do
      string
      |> String.split("")
      |> Enum.map(&String.capitalize/1)
      |> Enum.join("")
    end

    def reverse(string) do
      string
      |> String.split("")
      |> Enum.reverse()
      |> Enum.join("")
    end
  end
end
```

Explanation:

1. The `ComplexElixirCode` module is defined with various functions for different purposes.

2. The `greet/1` function greets a person by name.

3. The `sum/2` function sums two numbers.

4. The `factorial/1` function calculates the factorial of a given number.

5. The `fibonacci/1` function calculates the nth Fibonacci number.

6. The `Point` struct is defined to represent a point in a 2D space.

7. The `distance_between/2` function calculates the distance between two points.

8. The `Circle` struct is defined to represent a circle.

9. The `area_of_circle/1` function calculates the area of a circle.

10. The `Rectangle` struct is defined to represent a rectangle.

11. The `area_of_rectangle/1` function calculates the area of a rectangle.

12. The `Triangle` struct is defined to represent a triangle.

13. The `area_of_triangle/1` function calculates the area of a triangle.

14. The `ListUtils` module contains utility functions for working with lists.

15. The `map/3` function applies a function to each element of a list and returns a new list with the transformed elements.

16. The `filter/3` function filters a list based on a given condition and returns a new list with only the elements that satisfy the condition.

17. The `reduce/4` function reduces a list to a single value using a given function and an initial value.

18. The `StringUtils` module contains utility functions for working with strings.

19. The `capitalize/1` function capitalizes the first letter of each word in a string.

20. The `reverse/1` function reverses the order of characters in a string.