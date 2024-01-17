```kotlin
// Define a higher-order function that takes a lambda as an argument
fun calculate(operation: (Int, Int) -> Int, a: Int, b: Int): Int {
    return operation(a, b)
}

// Define a function to add two numbers
fun add(a: Int, b: Int): Int {
    return a + b
}

// Define a function to subtract two numbers
fun subtract(a: Int, b: Int): Int {
    return a - b
}

// Define a function to multiply two numbers
fun multiply(a: Int, b: Int): Int {
    return a * b
}

// Define a function to divide two numbers
fun divide(a: Int, b: Int): Int {
    return a / b
}

// Define a list of numbers
val numbers = listOf(1, 2, 3, 4, 5)

// Use the calculate function to add the numbers in the list
val sum = calculate(add, 1, 2) // 3
val sumOfList = numbers.reduce(add) // 15

// Use the calculate function to subtract the numbers in the list
val difference = calculate(subtract, 5, 2) // 3

// Use the calculate function to multiply the numbers in the list
val product = calculate(multiply, 2, 3) // 6
val productOfList = numbers.reduce(multiply) // 120

// Use the calculate function to divide the numbers in the list
val quotient = calculate(divide, 10, 2) // 5

// Print the results
println("Sum: $sum")
println("Sum of list: $sumOfList")
println("Difference: $difference")
println("Product: $product")
println("Product of list: $productOfList")
println("Quotient: $quotient")
```

Explanation:

1. We define higher-order functions calculate(), add(), subtract(), multiply(), and divide() that perform basic arithmetic operations on two integers.

2. The calculate() function takes a lambda (anonymous function) as an argument and two integers, a and b. It applies the lambda to a and b and returns the result.

3. We define a list of numbers called numbers.

4. We use the calculate() function with the add() lambda to calculate the sum of the numbers in the list. We also use the reduce() function on the numbers list with the add() lambda to achieve the same result.

5. We similarly use the calculate() and reduce() functions to calculate the difference, product, and quotient of the numbers in the list.

6. Finally, we print the results to the console.

This code demonstrates the use of higher-order functions, lambdas, and functional programming techniques in Kotlin. It's a more complex and differentiated code that showcases a variety of features and concepts in the Kotlin language.