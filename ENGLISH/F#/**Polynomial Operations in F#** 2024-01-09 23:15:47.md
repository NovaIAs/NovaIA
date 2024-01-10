```f#
// Define a module for working with polynomials
module Polynomial =

    // Define a type to represent a polynomial
    type Polynomial =
        | Constant of float
        | Variable of string
        | Sum of Polynomial * Polynomial
        | Product of Polynomial * Polynomial

    // Function to add two polynomials
    let add (p1: Polynomial) (p2: Polynomial) =
        match p1, p2 with
        | Constant c1, Constant c2 -> Constant (c1 + c2)
        | Variable v1, Variable v2 -> if v1 = v2 then Variable v1 else Sum (p1, p2)
        | Variable v, _ -> Sum (p1, p2)
        | _, Variable v -> Sum (p1, p2)
        | Sum (p11, p12), Sum (p21, p22) -> Sum (add p11 p21, add p12 p22)
        | Sum (p1, _), p2 -> Sum (p1, add p2 p1)
        | p1, Sum (_, p2) -> Sum (add p1 p2, p2)

    // Function to multiply two polynomials
    let multiply (p1: Polynomial) (p2: Polynomial) =
        match p1, p2 with
        | Constant c1, Constant c2 -> Constant (c1 * c2)
        | Variable v1, Variable v2 -> if v1 = v2 then Variable v1^2 else Product (p1, p2)
        | Variable v, _ -> Product (p1, p2)
        | _, Variable v -> Product (p1, p2)
        | Sum (p11, p12), Sum (p21, p22) -> add (multiply p11 p21) (multiply p12 p21) (multiply p11 p22) (multiply p12 p22)
        | Sum (p1, _), p2 -> add (multiply p1 p2) (multiply p1 p2)
        | p1, Sum (_, p2) -> add (multiply p1 p2) (multiply p1 p2)

    // Function to differentiate a polynomial
    let differentiate (p: Polynomial) =
        match p with
        | Constant _ -> Constant 0.0
        | Variable v -> Constant 1.0
        | Sum (p1, p2) -> add (differentiate p1) (differentiate p2)
        | Product (p1, p2) -> add (multiply (differentiate p1) p2) (multiply p1 (differentiate p2))

    // Function to evaluate a polynomial at a given value
    let evaluate (p: Polynomial) (x: float) =
        match p with
        | Constant c -> c
        | Variable v -> x
        | Sum (p1, p2) -> evaluate p1 x + evaluate p2 x
        | Product (p1, p2) -> evaluate p1 x * evaluate p2 x

// Define a polynomial to work with
let p = Sum (Constant 2.0, Product (Variable "x", Constant 3.0))

// Print the polynomial
printfn "%s" (sprintf "Polynomial: %A" p)

// Differentiate the polynomial
let dp = differentiate p

// Print the differentiated polynomial
printfn "%s" (sprintf "Differentiated Polynomial: %A" dp)

// Evaluate the polynomial at x = 2
let result = evaluate p 2.0

// Print the result
printfn "%s" (sprintf "Result at x = 2: %A" result)
```

Explanation:

This F# code defines a module called `Polynomial` that provides functions for working with polynomials. The module includes functions for adding, multiplying, differentiating, and evaluating polynomials.

The `Polynomial` type is defined as a discriminated union, which allows it to represent different types of polynomials. The possible cases are `Constant`, `Variable`, `Sum`, and `Product`.

The `add` function adds two polynomials together. It handles the different cases of polynomials by matching on the types of the polynomials being added. For example, if both polynomials are constants, it simply adds the two constants together. If one polynomial is a variable and the other is a constant, it adds the variable to the sum of the constant and itself.

The `multiply` function multiplies two polynomials together. It also handles the different cases of polynomials by matching on the types of the polynomials being multiplied. For example, if both polynomials are constants, it simply multiplies the two constants together. If one polynomial is a variable and the other is a constant, it multiplies the variable by the product of the constant and itself.

The `differentiate` function differentiates a polynomial. It also handles the different cases of polynomials by matching on the types of the polynomial being differentiated. For example, if the polynomial is a constant, it simply returns a constant of 0. If the polynomial is a variable, it returns a constant of 1.

The `evaluate` function evaluates a polynomial at a given value. It also handles the different cases of polynomials by matching on the types of the polynomial being evaluated. For example, if the polynomial is a constant, it simply returns the constant. If the polynomial is a variable, it returns the value of the variable at the given value.

The code then defines a polynomial `p` using the `Sum` and `Product` constructors. It prints the polynomial, differentiates it, prints the differentiated polynomial, evaluates it at `x = 2.0`, and prints the result.

This code demonstrates how to work with polynomials in F# using the `Polynomial` module. It shows how to add, multiply, differentiate, and evaluate polynomials.