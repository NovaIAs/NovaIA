```kotlin
// A simple Kotlin program to demonstrate the use of a lambda expression as an argument to a function

// Define a function that takes a lambda expression as an argument
fun applyOperation(operation: (Int, Int) -> Int) {
    // Call the lambda expression with two numbers and print the result
    val result = operation(10, 20)
    println("Result: $result")
}

// Define a lambda expression that adds two numbers
val addLambda: (Int, Int) -> Int = { a, b -> a + b }

// Define a lambda expression that multiplies two numbers
val multiplyLambda: (Int, Int) -> Int = { a, b -> a * b }

// Call the applyOperation() function with the addLambda lambda expression
applyOperation(addLambda)

// Call the applyOperation() function with the multiplyLambda lambda expression
applyOperation(multiplyLambda)

// Output:
// Result: 30
// Result: 200
```

**Explanation:**

This Kotlin program demonstrates the use of a lambda expression as an argument to a function. A lambda expression is a small anonymous function that can be passed as an argument to a function or stored in a variable.

The program defines a function called applyOperation() that takes a lambda expression as an argument. The lambda expression is a function that takes two integers as arguments and returns an integer.

The program then defines two lambda expressions: addLambda and multiplyLambda. The addLambda lambda expression adds two numbers, and the multiplyLambda lambda expression multiplies two numbers.

Finally, the program calls the applyOperation() function with the addLambda and multiplyLambda lambda expressions as arguments. The applyOperation() function calls the lambda expression with two numbers and prints the result.

The output of the program is:

```
Result: 30
Result: 200
```

This shows that the lambda expressions were successfully passed as arguments to the applyOperation() function and that the lambda expressions were executed correctly.