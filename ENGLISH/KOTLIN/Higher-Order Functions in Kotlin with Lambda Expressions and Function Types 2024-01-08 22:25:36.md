```kotlin
// Kotlin program to demonstrate the use of higher-order functions
// with lambda expressions and function types

// Function type alias for a function that takes two integers and returns an integer
typealias IntBinaryOperator = (Int, Int) -> Int

// Function type alias for a function that takes an integer and returns a boolean
typealias IntPredicate = (Int) -> Boolean

// Function type alias for a function that takes an integer and returns nothing
typealias IntConsumer = (Int) -> Unit

// Function that takes a list of integers and an IntBinaryOperator and returns a new list
// containing the results of applying the operator to each pair of consecutive elements in the list
fun <T> zipWith(list: List<T>, op: IntBinaryOperator): List<Int> {
    val result = mutableListOf<Int>()
    for (i in 0 until list.size - 1) {
        result.add(op(list[i], list[i + 1]))
    }
    return result
}

// Function that takes a list of integers and an IntPredicate and returns a new list
// containing only the elements that satisfy the predicate
fun <T> filter(list: List<T>, predicate: IntPredicate): List<Int> {
    val result = mutableListOf<Int>()
    for (i in list) {
        if (predicate(i)) {
            result.add(i)
        }
    }
    return result
}

// Function that takes a list of integers and an IntConsumer and performs the given action
// on each element in the list
fun <T> forEach(list: List<T>, action: IntConsumer) {
    for (i in list) {
        action(i)
    }
}

// Main function
fun main(args: Array<String>) {
    // Create a list of integers
    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    // Use the zipWith function to calculate the sum of each pair of consecutive elements in the list
    val sums = zipWith(numbers, { a, b -> a + b })

    // Use the filter function to select only the even numbers from the list
    val evenNumbers = filter(numbers, { it % 2 == 0 })

    // Use the forEach function to print each element in the list to the console
    forEach(numbers, { println(it) })

    // Print the results
    println("Sums: $sums")
    println("Even numbers: $evenNumbers")
}
```

Explanation:

1. We define three function type aliases:

    * `IntBinaryOperator`: A function that takes two integers as arguments and returns an integer.
    * `IntPredicate`: A function that takes an integer as an argument and returns a boolean.
    * `IntConsumer`: A function that takes an integer as an argument and returns nothing.

2. We define three functions:

    * `zipWith`: Takes a list of integers and an `IntBinaryOperator` as arguments and returns a new list containing the results of applying the operator to each pair of consecutive elements in the list.
    * `filter`: Takes a list of integers and an `IntPredicate` as arguments and returns a new list containing only the elements that satisfy the predicate.
    * `forEach`: Takes a list of integers and an `IntConsumer` as arguments and performs the given action on each element in the list.

3. In the `main` function, we:

    * Create a list of integers.
    * Use the `zipWith` function to calculate the sum of each pair of consecutive elements in the list and store the results in a new list.
    * Use the `filter` function to select only the even numbers from the list and store the results in a new list.
    * Use the `forEach` function to print each element in the list to the console.
    * Print the results.

This code demonstrates the use of higher-order functions in Kotlin with lambda expressions and function types. Higher-order functions are functions that take other functions as arguments or return functions as results. Lambda expressions are a concise way of writing anonymous functions. Function types are used to define the types of functions that can be passed as arguments to other functions.