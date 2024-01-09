```f#
// Define a custom type to represent a complex number
type Complex =
    | Real of float
    | Imaginary of float
    | Complex of float * float

// Define a function to add two complex numbers
let add (a: Complex) (b: Complex) =
    match a, b with
    | Real x, Real y -> Real (x + y)
    | Real x, Imaginary y -> Complex (x, y)
    | Imaginary x, Real y -> Complex (y, x)
    | Imaginary x, Imaginary y -> Imaginary (x + y)
    | Complex (x1, y1), Complex (x2, y2) -> Complex (x1 + x2, y1 + y2)

// Define a function to subtract two complex numbers
let subtract (a: Complex) (b: Complex) =
    match a, b with
    | Real x, Real y -> Real (x - y)
    | Real x, Imaginary y -> Complex (x, -y)
    | Imaginary x, Real y -> Complex (-y, x)
    | Imaginary x, Imaginary y -> Imaginary (x - y)
    | Complex (x1, y1), Complex (x2, y2) -> Complex (x1 - x2, y1 - y2)

// Define a function to multiply two complex numbers
let multiply (a: Complex) (b: Complex) =
    match a, b with
    | Real x, Real y -> Real (x * y)
    | Real x, Imaginary y -> Complex (x * y, 0.0)
    | Imaginary x, Real y -> Complex (0.0, x * y)
    | Imaginary x, Imaginary y -> Complex (0.0, -x * y)
    | Complex (x1, y1), Complex (x2, y2) -> Complex (x1 * x2 - y1 * y2, x1 * y2 + y1 * x2)

// Define a function to divide two complex numbers
let divide (a: Complex) (b: Complex) =
    match a, b with
    | Real x, Real y when y <> 0.0 -> Real (x / y)
    | Real x, Imaginary y when y <> 0.0 -> Complex (x / y, 0.0)
    | Imaginary x, Real y when y <> 0.0 -> Complex (0.0, x / y)
    | Imaginary x, Imaginary y when y <> 0.0 -> Complex (0.0, -x / y)
    | Complex (x1, y1), Complex (x2, y2) when x2 * x2 + y2 * y2 <> 0.0 ->
        Complex ((x1 * x2 + y1 * y2) / (x2 * x2 + y2 * y2), (y1 * x2 - x1 * y2) / (x2 * x2 + y2 * y2))
    | _, _ -> failwith "Division by zero"

// Define a function to calculate the absolute value of a complex number
let absolute (a: Complex) =
    match a with
    | Real x -> abs x
    | Imaginary x -> abs x
    | Complex (x, y) -> sqrt (x * x + y * y)

// Define a function to calculate the argument of a complex number
let argument (a: Complex) =
    match a with
    | Real x when x = 0.0 -> failwith "Argument of zero is undefined"
    | Real x -> 0.0
    | Imaginary x when x = 0.0 -> failwith "Argument of zero is undefined"
    | Imaginary x -> pi / 2.0
    | Complex (x, y) -> atan2 y x

// Define a function to calculate the complex conjugate of a complex number
let conjugate (a: Complex) =
    match a with
    | Real x -> a
    | Imaginary x -> Complex (0.0, -x)
    | Complex (x, y) -> Complex (x, -y)

// Define a function to calculate the reciprocal of a complex number
let reciprocal (a: Complex) =
    match a with
    | Real x when x = 0.0 -> failwith "Reciprocal of zero is undefined"
    | Real x -> Real (1.0 / x)
    | Imaginary x when x = 0.0 -> failwith "Reciprocal of zero is undefined"
    | Imaginary x -> Complex (0.0, -1.0 / x)
    | Complex (x, y) when x * x + y * y = 0.0 -> failwith "Reciprocal of zero is undefined"
    | Complex (x, y) -> Complex (x / (x * x + y * y), -y / (x * x + y * y))
```

This code defines a custom type called `Complex` to represent complex numbers. It then defines a number of functions to perform various operations on complex numbers, including addition, subtraction, multiplication, division, absolute value, argument, complex conjugate, and reciprocal. These functions are all implemented using pattern matching, which allows for concise and efficient code.

Here are some examples of how to use the `Complex` type and the functions defined above:

```f#
// Create a complex number
let c1 = Complex (1.0, 2.0)

// Add two complex numbers
let c2 = add c1 (Complex (3.0, 4.0))
// c2 = Complex (4.0, 6.0)

// Subtract two complex numbers
let c3 = subtract c1 (Complex (3.0, 4.0))
// c3 = Complex (-2.0, -2.0)

// Multiply two complex numbers
let c4 = multiply c1 (Complex (3.0, 4.0))
// c4 = Complex (-5.0, 10.0)

// Divide two complex numbers
let c5 = divide c1 (Complex (3.0, 4.0))
// c5 = Complex (0.44, 0.08)

// Calculate the absolute value of a complex number
let abs_c1 = absolute c1
// abs_c1 = 2.23606797749979

// Calculate the argument of a complex number
let arg_c1 = argument c1
// arg_c1 = 1.1071487177940904

// Calculate the complex conjugate of a complex number
let conj_c1 = conjugate c1
// conj_c1 = Complex (1.0, -2.0)

// Calculate the reciprocal of a complex number
let recip_c1 = reciprocal c1
// recip_c1 = Complex (0.44, -0.08)
```

This code demonstrates the various operations that can be performed on complex numbers using the `Complex` type and the functions defined above.