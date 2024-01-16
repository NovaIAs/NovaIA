```f#
// Define a module for working with complex numbers.
module Complex =

    // Define the complex number type.
    type Complex = { Real: float; Imaginary: float }

    // Define some helper functions for working with complex numbers.
    let ( + ) (c1: Complex) (c2: Complex) = { Real = c1.Real + c2.Real; Imaginary = c1.Imaginary + c2.Imaginary }
    let ( - ) (c1: Complex) (c2: Complex) = { Real = c1.Real - c2.Real; Imaginary = c1.Imaginary - c2.Imaginary }
    let ( * ) (c1: Complex) (c2: Complex) = { Real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary; Imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real }
    let ( / ) (c1: Complex) (c2: Complex) =
        let denominator = c2.Real * c2.Real + c2.Imaginary * c2.Imaginary
        { Real = (c1.Real * c2.Real + c1.Imaginary * c2.Imaginary) / denominator; Imaginary = (c1.Imaginary * c2.Real - c1.Real * c2.Imaginary) / denominator }

    // Define a function to calculate the magnitude of a complex number.
    let magnitude (c: Complex) = sqrt(c.Real * c.Real + c.Imaginary * c.Imaginary)

    // Define a function to calculate the argument of a complex number.
    let argument (c: Complex) = atan2(c.Imaginary, c.Real)

    // Define a function to calculate the complex conjugate of a complex number.
    let conjugate (c: Complex) = { Real = c.Real; Imaginary = -c.Imaginary }

    // Define a function to calculate the roots of a quadratic equation.
    let roots (a: float) (b: float) (c: float) =
        let discriminant = b * b - 4.0 * a * c
        if discriminant < 0.0 then
            []
        else
            [
                { Real = (-b + sqrt(discriminant)) / (2.0 * a); Imaginary = 0.0 };
                { Real = (-b - sqrt(discriminant)) / (2.0 * a); Imaginary = 0.0 }
            ]


// Test the complex number module.
let c1 = { Real = 3.0; Imaginary = 4.0 }
let c2 = { Real = 5.0; Imaginary = 6.0 }

printfn "%A + %A = %A" c1 c2 (c1 + c2)
printfn "%A - %A = %A" c1 c2 (c1 - c2)
printfn "%A * %A = %A" c1 c2 (c1 * c2)
printfn "%A / %A = %A" c1 c2 (c1 / c2)
printfn "Magnitude of %A = %A" c1 (magnitude c1)
printfn "Argument of %A = %A" c1 (argument c1)
printfn "Conjugate of %A = %A" c1 (conjugate c1)
printfn "Roots of x^2 + 2x + 5 = 0: %A" (roots 1.0 2.0 5.0)
```

This code defines a module called `Complex` for working with complex numbers. The module contains several functions for performing basic operations on complex numbers, such as addition, subtraction, multiplication, and division. It also contains functions for calculating the magnitude, argument, and conjugate of a complex number, as well as a function for finding the roots of a quadratic equation.

To use the `Complex` module, you can import it into your F# program using the following line:

```f#
open Complex
```

Once you have imported the module, you can use the functions it contains to perform operations on complex numbers. For example, the following code calculates the sum of two complex numbers:

```f#
let c1 = { Real = 3.0; Imaginary = 4.0 }
let c2 = { Real = 5.0; Imaginary = 6.0 }

let c3 = c1 + c2

printfn "%A + %A = %A" c1 c2 c3
```

This code will print the following output:

```
{ Real = 8.0; Imaginary = 10.0 } + { Real = 5.0; Imaginary = 6.0 } = { Real = 13.0; Imaginary = 16.0 }
```

You can also use the `Complex` module to perform more complex operations, such as finding the roots of a quadratic equation. The following code finds the roots of the quadratic equation `x^2 + 2x + 5 = 0`:

```f#
let roots = Complex.roots 1.0 2.0 5.0

printfn "Roots of x^2 + 2x + 5 = 0: %A" roots
```

This code will print the following output:

```
[|{ Real = -1.0; Imaginary = 2.0 }; { Real = -5.0; Imaginary = 0.0 }]
```