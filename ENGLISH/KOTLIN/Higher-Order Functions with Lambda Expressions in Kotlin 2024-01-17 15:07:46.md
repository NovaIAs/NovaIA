```kotlin
// Kotlin code to demonstrate use of Higher Order Functions

// Using lambda expression to sort the list of strings
val sortedList = listOf("Apple", "Orange", "Banana", "Pineapple", "Grapes")
    .sortedWith(compareBy { it.length }) // Sort based on the length of the string
println(sortedList) // [Apple, Orange, Grapes, Banana, Pineapple]

// Function to sum all the numbers in a list
fun sum(list: List<Int>): Int {
    var sum = 0
    for (num in list) {
        sum += num
    }
    return sum
}

// Using lambda expression to sum the numbers in a list
val numbers = listOf(1, 2, 3, 4, 5)
val sumOfNumbers = numbers.sumBy { it } // Sum the numbers
println(sumOfNumbers) // 15

// Function to find all the even numbers in a list
fun findEvenNumbers(list: List<Int>): List<Int> {
    val evenNumbers = mutableListOf<Int>()
    for (num in list) {
        if (num % 2 == 0) {
            evenNumbers.add(num)
        }
    }
    return evenNumbers
}

// Using lambda expression to find all the even numbers in a list
val evenNumbers = numbers.filter { it % 2 == 0 } // Filter the even numbers
println(evenNumbers) // [2, 4]

// Function to transform each element of a list using a lambda expression
fun transformList(list: List<String>): List<String> {
    val transformedList = mutableListOf<String>()
    for (str in list) {
        transformedList.add(str.toUpperCase())
    }
    return transformedList
}

// Using lambda expression to transform each element of a list
val transformedList = listOf("Apple", "Orange", "Banana", "Pineapple", "Grapes")
    .map { it.toUpperCase() } // Transform each string to uppercase
println(transformedList) // [APPLE, ORANGE, BANANA, PINEAPPLE, GRAPES]
```

**Explanation:**

1. **Using Lambda Expressions to Sort a List**:
   - The `sortedWith()` function is used to sort the list of strings based on a custom sorting criteria.
   - The lambda expression `{ it.length }` is used to specify the sorting criteria, which is the length of the string.
   - The sorted list is then printed using `println()`.

2. **Using a Function to Sum Numbers in a List**:
   - The `sum()` function is defined to calculate the sum of all the numbers in a list.
   - The `sumBy()` function is then used to sum the numbers in the `numbers` list using the lambda expression `{ it }`.
   - The result of the summation is printed using `println()`.

3. **Using Lambda Expressions to Find Even Numbers**:
   - The `findEvenNumbers()` function is defined to find all the even numbers in a list.
   - The `filter()` function is then used to filter the even numbers from the `numbers` list using the lambda expression `{ it % 2 == 0 }`.
   - The list of even numbers is printed using `println()`.

4. **Using Lambda Expressions to Transform List Elements**:
   - The `transformList()` function is defined to transform each element of a list using a lambda expression.
   - The `map()` function is then used to transform each string in the `transformedList` list to uppercase using the lambda expression `{ it.toUpperCase() }`.
   - The transformed list is printed using `println()`.

This code demonstrates the use of lambda expressions in various higher-order functions in Kotlin to perform common operations like sorting, summing, filtering, and transforming list elements.