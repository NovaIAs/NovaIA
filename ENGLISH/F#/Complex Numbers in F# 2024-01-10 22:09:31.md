```f#
// Create a module to encapsulate the code
module ModuleName =

    // Define a type to represent a complex number
    type Complex = { Real: float; Imaginary: float }

    // Define a function to add two complex numbers
    let Add (c1: Complex) (c2: Complex) =
        { Real = c1.Real + c2.Real; Imaginary = c1.Imaginary + c2.Imaginary }

    // Define a function to subtract two complex numbers
    let Subtract (c1: Complex) (c2: Complex) =
        { Real = c1.Real - c2.Real; Imaginary = c1.Imaginary - c2.Imaginary }

    // Define a function to multiply two complex numbers
    let Multiply (c1: Complex) (c2: Complex) =
        { Real = c1.Real * c2.Real - c1.Imaginary * c2.Imaginary;
          Imaginary = c1.Real * c2.Imaginary + c1.Imaginary * c2.Real }

    // Define a function to divide two complex numbers
    let Divide (c1: Complex) (c2: Complex) =
        let denominator = c2.Real * c2.Real + c2.Imaginary * c2.Imaginary
        { Real = (c1.Real * c2.Real + c1.Imaginary * c2.Imaginary) / denominator;
          Imaginary = (c1.Imaginary * c2.Real - c1.Real * c2.Imaginary) / denominator }

    // Define a function to calculate the absolute value of a complex number
    let Abs (c: Complex) =
        sqrt (c.Real * c.Real + c.Imaginary * c.Imaginary)

    // Define a function to calculate the argument of a complex number
    let Arg (c: Complex) =
        atan2 (c.Imaginary, c.Real)

    // Define a function to conjugate a complex number
    let Conjugate (c: Complex) =
        { Real = c.Real; Imaginary = -c.Imaginary }

    // Define a function to raise a complex number to a power
    let Pow (c: Complex) (n: int) =
        if n = 0 then
            { Real = 1.0; Imaginary = 0.0 }
        elif n > 0 then
            let result = Pow c (n / 2)
            Multiply result result
        else
            let result = Pow c (-n)
            Divide { Real = 1.0; Imaginary = 0.0 } result

// Use the module
ModuleName.Add { Real = 1.0; Imaginary = 2.0 } { Real = 3.0; Imaginary = 4.0 }
```

This code defines a module named `ModuleName` that contains a type and several functions for working with complex numbers. The type `Complex` represents a complex number with two fields, `Real` and `Imaginary`. The functions include addition, subtraction, multiplication, division, absolute value, argument, conjugation, and exponentiation.

To use the module, you can import it into your own code using the `open` keyword:

```f#
open ModuleName
```

You can then use the functions and type defined in the module in your own code. For example, to add two complex numbers, you can use the `Add` function:

```f#
let c1 = { Real = 1.0; Imaginary = 2.0 }
let c2 = { Real = 3.0; Imaginary = 4.0 }
let c3 = Add c1 c2
```

The result of this code is a complex number with a real part of 4.0 and an imaginary part of 6.0.