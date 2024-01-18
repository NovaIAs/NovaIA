```f#

type Expression =
    | Number of int
    | Variable of string
    | Add of Expression * Expression
    | Subtract of Expression * Expression
    | Multiply of Expression * Expression
    | Divide of Expression * Expression
    | Power of Expression * Expression

let rec eval (expr: Expression) :int=
    match expr with
    | Number n -> n
    | Variable x -> raise (Failure ("Unknown variable: " + x))
    | Add (e1, e2) -> (eval e1) + (eval e2)
    | Subtract (e1, e2) -> (eval e1) - (eval e2)
    | Multiply (e1, e2) -> (eval e1) * (eval e2)
    | Divide (e1, e2) -> (eval e1) / (eval e2)
    | Power (e1, e2) -> (eval e1) ** (eval e2)

let rec diff (expr: Expression) (variable: string) :Expression=
    match expr with
    | Number n -> Number 0
    | Variable x -> if x = variable then Number 1 else Number 0
    | Add (e1, e2) -> Add (diff e1 variable, diff e2 variable)
    | Subtract (e1, e2) -> Subtract (diff e1 variable, diff e2 variable)
    | Multiply (e1, e2) -> Add (Multiply (diff e1 variable, e2), Multiply (e1, diff e2 variable))
    | Divide (e1, e2) -> Divide (Subtract (Multiply (diff e1 variable, e2), Multiply (e1, diff e2 variable)), Power (e2, Number 2))
    | Power (e1, e2) -> Multiply (Multiply (e1, Power (e1, Subtract (e2, Number 1))), diff e2 variable)

let expr = Add (Multiply (Variable "x", Variable "y"), Power (Variable "z", Number 2))

printfn "Expression: %s" (expr |> string)
printfn "Value of expression: %d" (expr |> eval)

let derivative = diff expr "x"

printfn "Derivative of expression with respect to x: %s" (derivative |> string)
printfn "Value of derivative at x = 2, y = 3, z = 4: %d" (derivative |> eval ["x"=2;"y"=3;"z"=4])

```

This code implements symbolic differentiation of mathematical expressions in F#. Here's a detailed explanation:

1. **Expression Data Type:**
   - The `Expression` type represents mathematical expressions. It includes constructors for numbers, variables, addition, subtraction, multiplication, division, and exponentiation.

2. **Evaluation Function (`eval`):**
   - The `eval` function evaluates an expression and returns its numerical value. It recursively evaluates subexpressions based on their constructors.

3. **Differentiation Function (`diff`):**
   - The `diff` function differentiates an expression with respect to a given variable `variable`. It recursively applies differentiation rules to subexpressions.

4. **Example Expression:**
   - The `expr` variable is defined as a complex mathematical expression involving variables "x", "y", and "z".

5. **Evaluation and Differentiation:**
   - The `eval` function is used to evaluate the expression `expr` and print its value.
   - The `diff` function is used to differentiate the expression `expr` with respect to "x" and assign the result to the `derivative` variable.
   - The `eval` function is then used to evaluate the derivative at specific values of "x", "y", and "z".

This code demonstrates the symbolic differentiation of a complex mathematical expression, showing the expression, its value, the derivative with respect to a variable, and the value of the derivative at specified variable values.