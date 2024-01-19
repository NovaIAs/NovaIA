```f#
// This code demonstrates a complex and differentiated F# program that performs various operations.

// Defining a data type to represent a geometric shape
type Shape =
    | Circle of float // A circle with a given radius
    | Rectangle of float * float // A rectangle with given width and height
    | Triangle of float * float * float // A triangle with given side lengths

// Function to calculate the area of a shape
let calculateArea shape =
    match shape with
    | Circle radius -> PI * radius ** 2
    | Rectangle (width, height) -> width * height
    | Triangle (a, b, c) -> 0.5 * a * b

// Function to calculate the perimeter of a shape
let calculatePerimeter shape =
    match shape with
    | Circle radius -> 2.0 * PI * radius
    | Rectangle (width, height) -> 2.0 * (width + height)
    | Triangle (a, b, c) -> a + b + c

// Function to print the details of a shape
let printShapeDetails shape =
    match shape with
    | Circle radius -> printf "Circle with radius: %.2f" radius
    | Rectangle (width, height) -> printf "Rectangle with width: %.2f and height: %.2f" width height
    | Triangle (a, b, c) -> printf "Triangle with side lengths: %.2f, %.2f, %.2f" a b c

// Creating a list of shapes
let shapes = [
    Circle 5.0;
    Rectangle (3.0, 4.0);
    Triangle (6.0, 8.0, 10.0);
]

// Iterating over the list of shapes and calculating their areas and perimeters
for shape in shapes do
    printfn "Area of %A: %.2f" shape (calculateArea shape);
    printfn "Perimeter of %A: %.2f" shape (calculatePerimeter shape);
    printShapeDetails shape;
    printfn ""
done
```