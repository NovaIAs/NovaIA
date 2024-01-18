```f#
// Define a discriminated union to represent different types of shapes.
type Shape =
  | Circle of float // Represents a circle with a radius.
  | Rectangle of float * float // Represents a rectangle with a width and a height.
  | Triangle of float * float * float // Represents a triangle with three side lengths.

// Define a function to calculate the area of a shape.
let area shape =
  match shape with
  | Circle radius -> PI * radius * radius
  | Rectangle (width, height) -> width * height
  | Triangle (a, b, c) ->
      let s = (a + b + c) / 2.0
      sqrt (s * (s - a) * (s - b) * (s - c))

// Define a function to calculate the perimeter of a shape.
let perimeter shape =
  match shape with
  | Circle radius -> 2.0 * PI * radius
  | Rectangle (width, height) -> 2.0 * (width + height)
  | Triangle (a, b, c) -> a + b + c

// Define a function to print the details of a shape.
let printShape shape =
  printfn "%A" shape // Use the "%A" format specifier to print the shape's details.

// Create a list of shapes.
let shapes = [
  Circle 3.0;
  Rectangle (4.0, 5.0);
  Triangle (6.0, 7.0, 8.0);
]

// Iterate over the list of shapes and print their details.
List.iter printShape shapes
```

Explanation:

1. We define a discriminated union named `Shape` to represent different types of shapes. It has three cases: `Circle`, `Rectangle`, and `Triangle`. Each case has its own data representation.

2. We define a function named `area` to calculate the area of a shape. It uses pattern matching to handle each case of the `Shape` discriminated union and calculates the area accordingly.

3. We define a function named `perimeter` to calculate the perimeter of a shape. It also uses pattern matching to handle each case of the `Shape` discriminated union and calculates the perimeter accordingly.

4. We define a function named `printShape` to print the details of a shape. It uses the "%A" format specifier to print the shape's details in a human-readable format.

5. We create a list named `shapes` containing three different shapes: a circle, a rectangle, and a triangle.

6. We use the `List.iter` function to iterate over the list of shapes and call the `printShape` function on each shape to print its details.

This code demonstrates the use of discriminated unions, pattern matching, and higher-order functions in F#. It also showcases how to define custom data types and functions to operate on them.