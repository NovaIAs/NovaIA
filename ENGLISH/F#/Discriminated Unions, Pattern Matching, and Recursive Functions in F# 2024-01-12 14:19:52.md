```
// This F# code demonstrates the use of discriminated unions (enums), pattern matching, and recursive functions

// Define a discriminated union to represent the different types of shapes
type Shape =
 | Circle of float  // Circle with radius
 | Square of float  // Square with side length
 | Rectangle of float * float  // Rectangle with width and height
 | Triangle of float * float * float  // Triangle with side lengths

// Define a function to calculate the area of a shape
let area shape =
    match shape with
    | Circle radius -> radius * radius * Math.PI
    | Square side -> side * side
    | Rectangle (width, height) -> width * height
    | Triangle (a, b, c) -> 
        let s = (a + b + c) / 2.0
        Math.Sqrt (s * (s - a) * (s - b) * (s - c))

// Define a function to print a shape to the console
let printShape shape =
    match shape with
    | Circle radius -> Printf.printfn "Circle with radius %.2f" radius
    | Square side -> Printf.printfn "Square with side length %.2f" side
    | Rectangle (width, height) -> Printf.printfn "Rectangle with width %.2f and height %.2f" width height
    | Triangle (a, b, c) -> Printf.printfn "Triangle with sides %.2f, %.2f, and %.2f" a b c

// Define a recursive function to generate a list of random shapes
let rec generateShapes n =
    if n = 0 then []
    else
        let shape = 
            match Random.Next(4) with
            | 0 -> Circle (Random.Next(10.0))
            | 1 -> Square (Random.Next(10.0))
            | 2 -> Rectangle (Random.Next(10.0), Random.Next(10.0))
            | 3 -> Triangle (Random.Next(10.0), Random.Next(10.0), Random.Next(10.0))
        |> shape
        shape :: generateShapes (n-1)

// Generate a list of 10 random shapes
let shapes = generateShapes 10

// Print the shapes and their areas
for shape in shapes do
    printShape shape
    Printf.printfn "Area: %.2f" (area shape)
```

Explanation:

1. **Discriminated Unions (Enums):**
   - We define a discriminated union (`Shape`) to represent the different types of shapes.
   - Each case of the union represents a different shape type, e.g., `Circle`, `Square`, `Rectangle`, and `Triangle`.
   - Each case has its own data fields to store the specific properties of that shape.

2. **Pattern Matching:**
   - We use pattern matching extensively to handle different cases of the `Shape` union.
   - The `match` expression allows us to deconstruct a value of the `Shape` union and access its data fields.

3. **Recursive Function:**
   - We define a recursive function `generateShapes` to generate a list of random shapes.
   - The function uses pattern matching to randomly select a shape type and generate a shape with random dimensions.

4. **Printing Shapes and Areas:**
   - We define a function `printShape` to print a shape to the console.
   - We also calculate and print the area of each shape using the `area` function.

5. **Program Flow:**
   - We generate a list of 10 random shapes using the `generateShapes` function.
   - We then iterate over the list of shapes and print each shape along with its area.

This code demonstrates the use of discriminated unions, pattern matching, and recursive functions in F#. It also showcases how these concepts can be used to create complex and interesting programs.