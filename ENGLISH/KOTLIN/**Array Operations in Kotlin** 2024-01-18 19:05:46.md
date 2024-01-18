```kotlin
// Import the necessary libraries.
import java.util.Scanner
import java.util.Arrays
import java.util.Random

// Define the main function.
fun main(args: Array<String>) {
    // Create a scanner object to read input from the console.
    val scanner = Scanner(System.`in`)

    // Create a random object to generate random numbers.
    val random = Random()

    // Define the size of the array.
    val size = 10

    // Create an array of integers.
    val array = IntArray(size)

    // Fill the array with random numbers.
    for (i in 0 until size) {
        array[i] = random.nextInt(100)
    }

    // Print the original array.
    println("Original array:")
    for (i in 0 until size) {
        print("${array[i]} ")
    }

    // Sort the array in ascending order.
    Arrays.sort(array)

    // Print the sorted array.
    println("\nSorted array:")
    for (i in 0 until size) {
        print("${array[i]} ")
    }

    // Find the maximum element in the array.
    val max = array[size - 1]

    // Find the minimum element in the array.
    val min = array[0]

    // Calculate the sum of all elements in the array.
    var sum = 0
    for (i in 0 until size) {
        sum += array[i]
    }

    // Calculate the average of all elements in the array.
    val average = sum / size.toDouble()

    // Print the maximum, minimum, sum, and average of the array.
    println("\nMaximum: $max")
    println("Minimum: $min")
    println("Sum: $sum")
    println("Average: $average")

    // Search for a specific element in the array using linear search.
    println("Enter an element to search for:")
    val element = scanner.nextInt()

    var found = false
    for (i in 0 until size) {
        if (array[i] == element) {
            found = true
            break
        }
    }

    // Print the result of the search.
    if (found) {
        println("Element found at index $i")
    } else {
        println("Element not found")
    }
}
```

This code performs a variety of operations on an array of integers, including filling the array with random numbers, sorting the array, finding the maximum and minimum elements, calculating the sum and average of the elements, searching for a specific element, and printing the results.

Here is a more detailed explanation of the code:

* **Line 1:** Import the necessary libraries.
* **Line 8:** Define the main function.
* **Line 11:** Create a scanner object to read input from the console.
* **Line 14:** Create a random object to generate random numbers.
* **Line 17:** Define the size of the array.
* **Line 20:** Create an array of integers.
* **Lines 22-26:** Fill the array with random numbers using a for loop.
* **Lines 28-32:** Print the original array using a for loop.
* **Lines 34-38:** Sort the array in ascending order using the `Arrays.sort()` function.
* **Lines 40-44:** Print the sorted array using a for loop.
* **Lines 46-47:** Find the maximum and minimum elements in the array using the `array[size - 1]` and `array[0]` expressions, respectively.
* **Lines 49-53:** Calculate the sum of all elements in the array using a for loop.
* **Lines 55-56:** Calculate the average of all elements in the array by dividing the sum by the size of the array.
* **Lines 58-62:** Print the maximum, minimum, sum, and average of the array.
* **Lines 64-72:** Search for a specific element in the array using linear search.
* **Lines 74-77:** Print the result of the search.

This code is a good example of how to use Kotlin to perform a variety of operations on an array of integers. It is also a good example of how to use loops, conditional statements, and functions in Kotlin.