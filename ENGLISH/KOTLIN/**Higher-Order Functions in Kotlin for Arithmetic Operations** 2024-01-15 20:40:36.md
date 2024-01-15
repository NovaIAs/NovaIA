```kotlin
// Defining a higher-order function that takes a function as an argument and returns another function
fun higherOrderFunction(operation: (Int, Int) -> Int): (Int, Int) -> Int {
    return { a, b -> operation(a, b) }
}

// Defining a function that adds two numbers
fun add(a: Int, b: Int): Int {
    return a + b
}

// Defining a function that subtracts two numbers
fun subtract(a: Int, b: Int): Int {
    return a - b
}

// Defining a function that multiplies two numbers
fun multiply(a: Int, b: Int): Int {
    return a * b
}

// Defining a function that divides two numbers
fun divide(a: Int, b: Int): Int {
    return a / b
}

// Calling the higher-order function and passing the add function as an argument
val addFunction = higherOrderFunction(::add)

// Calling the higher-order function and passing the subtract function as an argument
val subtractFunction = higherOrderFunction(::subtract)

// Calling the higher-order function and passing the multiply function as an argument
val multiplyFunction = higherOrderFunction(::multiply)

// Calling the higher-order function and passing the divide function as an argument
val divideFunction = higherOrderFunction(::divide)

// Using the returned function from the higher-order function to perform addition
val result1 = addFunction(10, 5)

// Using the returned function from the higher-order function to perform subtraction
val result2 = subtractFunction(10, 5)

// Using the returned function from the higher-order function to perform multiplication
val result3 = multiplyFunction(10, 5)

// Using the returned function from the higher-order function to perform division
val result4 = divideFunction(10, 5)

// Printing the results
println("Addition result: $result1")
println("Subtraction result: $result2")
println("Multiplication result: $result3")
println("Division result: $result4")
```

Explanation:

1. We define a higher-order function called `higherOrderFunction` that takes a function as an argument and returns another function. This allows us to pass a function as an argument to another function.

2. We define four functions: `add`, `subtract`, `multiply`, and `divide` that perform basic arithmetic operations.

3. We call the `higherOrderFunction` and pass each of the arithmetic functions as arguments. This returns a new function that performs the specific operation.

4. We use the returned functions to perform the arithmetic operations and store the results in variables.

5. Finally, we print the results to the console.

This code demonstrates the use of higher-order functions in Kotlin, which allows us to pass functions as arguments to other functions and create more concise and reusable code.