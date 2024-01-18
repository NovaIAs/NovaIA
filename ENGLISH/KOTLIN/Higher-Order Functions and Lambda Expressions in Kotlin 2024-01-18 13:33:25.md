```kotlin
// Creating a higher-order function that takes a lambda as an argument
fun calculate(operation: (Int, Int) -> Int, a: Int, b: Int): Int {
    return operation(a, b)
}

// Creating a lambda expression that adds two numbers
val addLambda: (Int, Int) -> Int = { a, b -> a + b }

// Creating a lambda expression that subtracts two numbers
val subtractLambda: (Int, Int) -> Int = { a, b -> a - b }

// Creating a lambda expression that multiplies two numbers
val multiplyLambda: (Int, Int) -> Int = { a, b -> a * b }

// Creating a lambda expression that divides two numbers
val divideLambda: (Int, Int) -> Int = { a, b -> a / b }

// Calling the calculate function with the add lambda expression
val additionResult = calculate(addLambda, 10, 20)

// Calling the calculate function with the subtract lambda expression
val subtractionResult = calculate(subtractLambda, 10, 20)

// Calling the calculate function with the multiply lambda expression
val multiplicationResult = calculate(multiplyLambda, 10, 20)

// Calling the calculate function with the divide lambda expression
val divisionResult = calculate(divideLambda, 10, 20)

// Printing the results
println("Addition Result: $additionResult")
println("Subtraction Result: $subtractionResult")
println("Multiplication Result: $multiplicationResult")
println("Division Result: $divisionResult")
```

Explanation:

1. We define a higher-order function called calculate that takes a lambda expression as an argument along with two integer parameters (a and b). This function will perform the specified operation on the two integers using the provided lambda expression and return the result.

2. We create four lambda expressions:

   - addLambda: This lambda expression adds two numbers.
   - subtractLambda: This lambda expression subtracts two numbers.
   - multiplyLambda: This lambda expression multiplies two numbers.
   - divideLambda: This lambda expression divides two numbers.

3. We call the calculate function with each of the lambda expressions and the values 10 and 20 as arguments. This calculates the addition, subtraction, multiplication, and division results of the two numbers using the respective lambda expressions.

4. Finally, we print the results to the console.

This code demonstrates the use of higher-order functions and lambda expressions in Kotlin to perform various operations on two numbers. It showcases the flexibility and power of lambda expressions in defining concise and reusable code blocks that can be passed around as arguments to other functions.