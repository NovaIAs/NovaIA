```kotlin
// Import the necessary libraries
import java.util.*
import kotlin.collections.ArrayList

// Define the main function
fun main(args: Array<String>) {
    // Create an ArrayList of integers
    val numbers = ArrayList<Int>()

    // Add some integers to the ArrayList
    numbers.add(1)
    numbers.add(2)
    numbers.add(3)
    numbers.add(4)
    numbers.add(5)

    // Iterate over the ArrayList using a for loop
    for (number in numbers) {
        // Print the current number
        println(number)
    }

    // Iterate over the ArrayList using a while loop
    var index = 0
    while (index < numbers.size) {
        // Print the current number
        println(numbers[index])

        // Increment the index
        index++
    }

    // Iterate over the ArrayList using a do-while loop
    index = 0
    do {
        // Print the current number
        println(numbers[index])

        // Increment the index
        index++
    } while (index < numbers.size)

    // Sort the ArrayList in ascending order
    numbers.sort()

    // Print the sorted ArrayList
    println(numbers)

    // Sort the ArrayList in descending order
    numbers.sortDescending()

    // Print the sorted ArrayList
    println(numbers)

    // Find the maximum value in the ArrayList
    val maxValue = numbers.max()

    // Print the maximum value
    println("Maximum value: $maxValue")

    // Find the minimum value in the ArrayList
    val minValue = numbers.min()

    // Print the minimum value
    println("Minimum value: $minValue")

    // Find the sum of the values in the ArrayList
    val sum = numbers.sum()

    // Print the sum
    println("Sum: $sum")

    // Find the average of the values in the ArrayList
    val average = numbers.average()

    // Print the average
    println("Average: $average")

    // Find the median of the values in the ArrayList
    val median = numbers.median()

    // Print the median
    println("Median: $median")

    // Find the mode of the values in the ArrayList
    val mode = numbers.mode()

    // Print the mode
    println("Mode: $mode")

    // Find the range of the values in the ArrayList
    val range = numbers.range()

    // Print the range
    println("Range: $range")

    // Find the variance of the values in the ArrayList
    val variance = numbers.variance()

    // Print the variance
    println("Variance: $variance")

    // Find the standard deviation of the values in the ArrayList
    val standardDeviation = numbers.standardDeviation()

    // Print the standard deviation
    println("Standard deviation: $standardDeviation")
}

// Define an extension function to find the median of an ArrayList of integers
fun ArrayList<Int>.median(): Int {
    // Sort the ArrayList in ascending order
    this.sort()

    // If the ArrayList has an even number of elements, the median is the average of the two middle elements
    if (this.size % 2 == 0) {
        return (this[this.size / 2 - 1] + this[this.size / 2]) / 2
    }
    // If the ArrayList has an odd number of elements, the median is the middle element
    else {
        return this[this.size / 2]
    }
}

// Define an extension function to find the mode of an ArrayList of integers
fun ArrayList<Int>.mode(): Int {
    // Create a HashMap to store the frequency of each element in the ArrayList
    val frequencyMap = HashMap<Int, Int>()

    // Iterate over the ArrayList and update the frequency of each element in the HashMap
    for (number in this) {
        if (frequencyMap.containsKey(number)) {
            frequencyMap[number] = frequencyMap[number]!! + 1
        } else {
            frequencyMap[number] = 1
        }
    }

    // Find the maximum frequency in the HashMap
    var maxFrequency = 0
    for (frequency in frequencyMap.values) {
        if (frequency > maxFrequency) {
            maxFrequency = frequency
        }
    }

    // Create an ArrayList to store the modes of the ArrayList
    val modes = ArrayList<Int>()

    // Iterate over the HashMap and add the elements with the maximum frequency to the ArrayList
    for (entry in frequencyMap.entries) {
        if (entry.value == maxFrequency) {
            modes.add(entry.key)
        }
    }

    // Return the ArrayList of modes
    return modes
}

// Define an extension function to find the range of an ArrayList of integers
fun ArrayList<Int>.range(): Int {
    // Find the maximum value in the ArrayList
    val maxValue = this.max()

    // Find the minimum value in the ArrayList
    val minValue = this.min()

    // Return the difference between the maximum and minimum values
    return maxValue - minValue
}

// Define an extension function to find the variance of an ArrayList of integers
fun ArrayList<Int>.variance(): Double {
    // Find the mean of the ArrayList
    val mean = this.average()

    // Create a variable to store the sum of the squared differences between each element and the mean
    var sumOfSquaredDifferences = 0.0

    // Iterate over the ArrayList and calculate the sum of the squared differences
    for (number in this) {
        sumOfSquaredDifferences += (number - mean) * (number - mean)
    }

    // Return the sum of the squared differences divided by the number of elements in the ArrayList
    return sumOfSquaredDifferences / this.size
}

// Define an extension function to find the standard deviation of an ArrayList of integers
fun ArrayList<Int>.standardDeviation(): Double {
    // Find the variance of the ArrayList
    val variance = this.variance()

    // Return the square root of the variance
    return Math.sqrt(variance)
}

```

**Explanation:**

This Kotlin code demonstrates various operations on an ArrayList of integers, including iterating over the list using different loops, sorting the list in ascending and descending order, finding the maximum, minimum, sum, average, median, mode, range, variance, and standard deviation of the values in the list.

Here's a detailed explanation of each part of the code:

1. **Importing Libraries:**
   - `import java.util.*`: Imports the `ArrayList` class from the Java standard library.
   - `import kotlin.collections.ArrayList`: Imports the `ArrayList` class from the Kotlin standard library.

2. **Main Function:**
   - `fun main(args: Array<String>)`: Defines the main function of the program.

3. **Creating an ArrayList:**
   - `val numbers = ArrayList<Int>()`: Creates an empty ArrayList named `numbers` to store integers.

4. **Adding Integers to the ArrayList:**
   - `numbers.add(1)`: Adds the integer `1` to the `numbers` list.
   - This is repeated for integers `2` to `5`, adding them to the list.

5. **Iterating Over the ArrayList:**
   - There are three different loops used to iterate over the `numbers` list:
     - **For Loop:**
       - `for (number in numbers)`: This loop iterates over each element of the `numbers` list and prints the current number.
     - **While Loop:**
       - `var index = 0`: Initializes an integer variable `index` to `0`.
       - `while (index < numbers.size)`: This loop continues as long as the value of `index` is less than the size of the `numbers` list. Inside the loop, the current number is printed, and `index` is incremented.
     - **Do-While Loop:**
       - `index = 0`: Initializes `index` to `0` again.
       - `do { ... } while (index < numbers.size)`: This loop is similar to the `while` loop, but it executes the code block inside the loop at least once, even if the condition `index < numbers.size` is initially false.

6. **Sorting the ArrayList:**
   - `numbers.sort()`: Sorts the `numbers` list in ascending order.
   - `numbers.sortDescending()`: Sorts the `numbers` list in descending order.

7. **Finding Maximum and Minimum Values:**
   - `val maxValue = numbers.max()`: Finds and stores the maximum value in the `numbers` list.
   - `val minValue = numbers.min()`: Finds and stores the minimum value in the `numbers` list.

8. **Calculating Sum, Average, Median, Mode, Range, Variance, and Standard Deviation:**
   - `val sum = numbers.sum()`: Calculates and stores the sum of all the values in the `numbers` list.
   - `val average = numbers.average()`: Calculates and stores the average of all the values in the `numbers` list.
   - `val median = numbers