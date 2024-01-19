```f#
// Define a discriminated union type representing different types of shapes.
type Shape =
    | Circle of float // A circle with a radius.
    | Rectangle of float * float // A rectangle with a width and a height.
    | Triangle of float * float * float // A triangle with three side lengths.

// Define a function to calculate the area of a shape.
let calculateArea shape =
    match shape with
    | Circle r -> Math.PI * r * r
    | Rectangle (w, h) -> w * h
    | Triangle (a, b, c) ->
        // Calculate the semiperimeter of the triangle.
        let s = (a + b + c) / 2.0

        // Use Heron's formula to calculate the area.
        Math.Sqrt (s * (s - a) * (s - b) * (s - c))

// Define a function to print the area of a shape.
let printArea shape =
    printfn "%f" (calculateArea shape)

// Create a list of shapes.
let shapes = [
    Circle 2.0;
    Rectangle (3.0, 4.0);
    Triangle (5.0, 6.0, 7.0)
]

// Print the area of each shape.
for shape in shapes do
    printArea shape
```

This code defines a discriminated union type called `Shape` that represents different types of shapes. It then defines a function called `calculateArea` that calculates the area of a shape. The function uses pattern matching to determine the type of shape and then uses the appropriate formula to calculate the area. The code also defines a function called `printArea` that prints the area of a shape. Finally, the code creates a list of shapes and then uses a `for` loop to print the area of each shape.