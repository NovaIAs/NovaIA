```f#
// Define a discriminated union type to represent various types of expressions
type Expr =
    | Var of string                    // Variable
    | Lit of int                       // Literal
    | Neg of Expr                      // Negation
    | Add of Expr * Expr               // Addition
    | Sub of Expr * Expr               // Subtraction
    | Mul of Expr * Expr               // Multiplication
    | Div of Expr * Expr               // Division

// Define a function to evaluate an expression
let rec eval expr =
    match expr with
    // Evaluate a variable by looking it up in the environment
    | Var v -> lookup v
    
    // Evaluate a literal as-is
    | Lit n -> n
    
    // Evaluate a negation by negating the evaluated subexpression
    | Neg e -> -eval e

    // Evaluate an addition by adding the evaluated subexpressions
    | Add (e1, e2) -> eval e1 + eval e2

    // Evaluate a subtraction by subtracting the evaluated subexpressions
    | Sub (e1, e2) -> eval e1 - eval e2

    // Evaluate a multiplication by multiplying the evaluated subexpressions
    | Mul (e1, e2) -> eval e1 * eval e2

    // Evaluate a division by dividing the evaluated subexpressions
    | Div (e1, e2) -> eval e1 / eval e2

// Define a function to look up a variable in the environment
let lookup v =
    // Retrieve the environment from the current context
    let env = Environment.current

    // Look up the variable in the environment
    try env.[v]
    with _ -> failwith $"error: variable '{v}' not found in environment"

// Define a module to manage the environment
module Environment =
    // Create a new environment with the given bindings
    let new bindings =
        bindings

    // Look up a variable in the environment
    let lookup v =
        try bindings.[v]
        with _ -> failwith $"error: variable '{v}' not found in environment"

    // Update the value of a variable in the environment
    let set v value =
        bindings.[v] <- value

// Create an initial environment with pre-defined variables
let initialEnv = Environment.new [
    ("x", 10),
    ("y", 20)
]

// Set the current environment for evaluation
Environment.current <- initialEnv

```