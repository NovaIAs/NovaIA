```f#
// Define a type to represent a complex number
type Complex = { Real: double; Imaginary: double }

// Define addition and subtraction operators for complex numbers
let ( + ) (c1: Complex) (c2: Complex) = { Real = c1.Real + c2.Real; Imaginary = c1.Imaginary + c2.Imaginary }
let ( - ) (c1: Complex) (c2: Complex) = { Real = c1.Real - c2.Real; Imaginary = c1.Imaginary - c2.Imaginary }

// Define multiplication and division operators for complex numbers
let ( * ) (c1: Complex) (c2: Complex) = { Real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary; Imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real }
let ( / ) (c1: Complex) (c2: Complex) = { Real = (c1.Real * c2.Real + c1.Imaginary * c2.Imaginary) / (c2.Real * c2.Real + c2.Imaginary * c2.Imaginary); Imaginary = (c1.Imaginary * c2.Real - c1.Real * c2.Imaginary) / (c2.Real * c2.Real + c2.Imaginary * c2.Imaginary) }

// Define a function to calculate the magnitude of a complex number
let magnitude (c: Complex) = sqrt(c.Real * c.Real + c.Imaginary * c.Imaginary)

// Define a function to calculate the argument of a complex number
let argument (c: Complex) = atan2(c.Imaginary, c.Real)

// Define a function to create a complex number from a magnitude and argument
let fromMagnitudeAndArgument (magnitude: double) (argument: double) = { Real = magnitude * cos(argument); Imaginary = magnitude * sin(argument) }

// Define a function to convert a complex number to a string
let toString (c: Complex) = sprintf "%.2f + %.2fi" c.Real c.Imaginary

// Define a function to test the complex number operations
let testComplexNumbers () =
    let c1 = { Real = 3.0; Imaginary = 4.0 }
    let c2 = { Real = 5.0; Imaginary = -2.0 }

    printfn "c1 = %s" (toString c1)
    printfn "c2 = %s" (toString c2)

    printfn "c1 + c2 = %s" (toString (c1 + c2))
    printfn "c1 - c2 = %s" (toString (c1 - c2))
    printfn "c1 * c2 = %s" (toString (c1 * c2))
    printfn "c1 / c2 = %s" (toString (c1 / c2))

    printfn "Magnitude of c1 = %f" (magnitude c1)
    printfn "Argument of c1 = %f" (argument c1)

    let c3 = fromMagnitudeAndArgument 5.0 0.5
    printfn "c3 = %s" (toString c3)

// Call the test function
testComplexNumbers()
```

This code defines a type to represent complex numbers, and defines operators for addition, subtraction, multiplication, and division of complex numbers. It also defines functions to calculate the magnitude and argument of a complex number, and to create a complex number from a magnitude and argument. Finally, it defines a function to test the complex number operations.

The code is complex and differentiated in the following ways:

* It defines a new type, `Complex`, to represent complex numbers.
* It defines operators for addition, subtraction, multiplication, and division of complex numbers.
* It defines functions to calculate the magnitude and argument of a complex number.
* It defines a function to create a complex number from a magnitude and argument.
* It defines a function to test the complex number operations.

The code is also well-commented and easy to read, making it easy to understand and modify.