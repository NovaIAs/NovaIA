```kotlin
// Import the necessary libraries
import java.util.*
import kotlin.math.*

// Define the main function
fun main(args: Array<String>) {
    // Create a list of integers
    val numbers = listOf(1, 2, 3, 4, 5)

    // Print the sum of the numbers
    println("The sum of the numbers is ${numbers.sum()}")

    // Print the average of the numbers
    println("The average of the numbers is ${numbers.average()}")

    // Print the maximum value of the numbers
    println("The maximum value of the numbers is ${numbers.max()}")

    // Print the minimum value of the numbers
    println("The minimum value of the numbers is ${numbers.min()}")

    // Create a map of strings to integers
    val map = mapOf("one" to 1, "two" to 2, "three" to 3)

    // Print the value associated with the key "two"
    println("The value associated with the key \"two\" is ${map["two"]}")

    // Check if the map contains the key "four"
    println("The map contains the key \"four\" is ${map.containsKey("four")}")

    // Create a set of strings
    val set = setOf("one", "two", "three")

    // Print the size of the set
    println("The size of the set is ${set.size}")

    // Check if the set contains the element "two"
    println("The set contains the element \"two\" is ${set.contains("two")}")

    // Create a function that takes two integers and returns their sum
    fun sum(a: Int, b: Int): Int {
        return a + b
    }

    // Call the sum function with the arguments 1 and 2
    val result = sum(1, 2)

    // Print the result
    println("The result of the sum function is $result")
}

// Define a class to represent a person
class Person(val name: String, val age: Int)

// Create a list of people
val people = listOf(
    Person("John", 25),
    Person("Mary", 30),
    Person("Bob", 35)
)

// Print the names of the people in the list
for (person in people) {
    println(person.name)
}

// Print the ages of the people in the list
for (person in people) {
    println(person.age)
}
```

**Explanation:**

This code is a demonstration of various features and functionalities of the Kotlin programming language. It covers data structures, control flow, functions, classes, and loops.

1. **Data Structures:**
   - `List<Int>`: A list of integers to store a collection of numbers.
   - `Map<String, Int>`: A map of strings to integers to associate keys with values.
   - `Set<String>`: A set of strings to store a collection of unique elements.

2. **Control Flow:**
   - `if` statement: Used to check a condition and execute a block of code if the condition is true.
   - `println()`: Used to print a value to the console.

3. **Functions:**
   - `sum()`: A function that takes two integers as arguments and returns their sum.

4. **Classes:**
   - `Person`: A class to represent a person with a name and age.

5. **Loops:**
   - `for` loop: Used to iterate over a collection of elements.

The code provides a comprehensive overview of Kotlin's capabilities and serves as a solid foundation for exploring more advanced concepts in the language.