```kotlin
// Kotlin program to demonstrate the use of higher-order functions

// Function that takes a function as an argument and returns a function
fun higherOrderFunction(operation: (Int, Int) -> Int): (Int, Int) -> Int {
    // Returning the passed function
    return operation
}

// Function to add two numbers
fun add(a: Int, b: Int): Int {
    return a + b
}

// Function to subtract two numbers
fun subtract(a: Int, b: Int): Int {
    return a - b
}

// Function to multiply two numbers
fun multiply(a: Int, b: Int): Int {
    return a * b
}

// Function to divide two numbers
fun divide(a: Int, b: Int): Int {
    return a / b
}

// Main function
fun main(args: Array<String>) {
    // Get the higher-order function for addition
    val addFunction = higherOrderFunction(::add)

    // Get the higher-order function for subtraction
    val subtractFunction = higherOrderFunction(::subtract)

    // Get the higher-order function for multiplication
    val multiplyFunction = higherOrderFunction(::multiply)

    // Get the higher-order function for division
    val divideFunction = higherOrderFunction(::divide)

    // Use the higher-order functions to perform operations
    println("Addition: ${addFunction(10, 20)}")
    println("Subtraction: ${subtractFunction(20, 10)}")
    println("Multiplication: ${multiplyFunction(10, 20)}")
    println("Division: ${divideFunction(20, 10)}")
}
```

**Explanation:**

The provided Kotlin code demonstrates the concept of higher-order functions. A higher-order function is a function that takes other functions as arguments or returns a function as its result.

In this code, the `higherOrderFunction` is a higher-order function that takes a function as an argument and returns another function. This allows us to create new functions based on existing ones.

We define four functions: `add`, `subtract`, `multiply`, and `divide`, which perform basic arithmetic operations.

The `higherOrderFunction` function is called four times to get higher-order functions for addition, subtraction, multiplication, and division.

These higher-order functions are then used to perform the actual operations on the numbers 10 and 20.

The output of the program is:

```
Addition: 30
Subtraction: 10
Multiplication: 200
Division: 2
```