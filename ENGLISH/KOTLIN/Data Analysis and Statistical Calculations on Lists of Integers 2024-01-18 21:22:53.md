```kotlin
// Import necessary libraries
import java.util.List
import java.util.Arrays
import java.util.Scanner

// Define a function to find the minimum element in a list of integers
fun findMinimum(list: List<Int>): Int {
    var min = list[0]
    for (i in 1 until list.size) {
        if (list[i] < min) {
            min = list[i]
        }
    }
    return min
}

// Define a function to find the maximum element in a list of integers
fun findMaximum(list: List<Int>): Int {
    var max = list[0]
    for (i in 1 until list.size) {
        if (list[i] > max) {
            max = list[i]
        }
    }
    return max
}

// Define a function to find the sum of a list of integers
fun findSum(list: List<Int>): Int {
    var sum = 0
    for (i in list) {
        sum += i
    }
    return sum
}

// Define a function to find the average of a list of integers
fun findAverage(list: List<Int>): Double {
    var sum = 0
    for (i in list) {
        sum += i
    }
    return sum.toDouble() / list.size
}

// Define a function to find the median of a list of integers
fun findMedian(list: List<Int>): Double {
    // Sort the list in ascending order
    list.sort()

    // If the list is of even length, the median is the average of the two middle elements
    if (list.size % 2 == 0) {
        return (list[list.size / 2 - 1] + list[list.size / 2]) / 2.toDouble()
    }

    // If the list is of odd length, the median is the middle element
    else {
        return list[list.size / 2].toDouble()
    }
}

// Define a function to find the mode of a list of integers
fun findMode(list: List<Int>): Int {
    // Create a map to store the frequency of each element in the list
    val frequencyMap = mutableMapOf<Int, Int>()

    // Iterate over the list and update the frequency of each element
    for (i in list) {
        frequencyMap[i] = frequencyMap.getOrDefault(i, 0) + 1
    }

    // Find the element with the highest frequency
    var maxFrequency = 0
    var mode = 0
    for (entry in frequencyMap) {
        if (entry.value > maxFrequency) {
            maxFrequency = entry.value
            mode = entry.key
        }
    }

    return mode
}

// Define a function to find the range of a list of integers
fun findRange(list: List<Int>): Int {
    return findMaximum(list) - findMinimum(list)
}

// Define a function to find the variance of a list of integers
fun findVariance(list: List<Int>): Double {
    // Find the mean of the list
    val mean = findAverage(list)

    // Calculate the variance
    var variance = 0.0
    for (i in list) {
        variance += (i - mean) * (i - mean)
    }
    variance /= (list.size - 1)

    return variance
}

// Define a function to find the standard deviation of a list of integers
fun findStandardDeviation(list: List<Int>): Double {
    return Math.sqrt(findVariance(list))
}

// Define a function to get user input
fun getUserInput(prompt: String): String {
    print(prompt)
    val scanner = Scanner(System.`in`)
    return scanner.nextLine()
}

// Define a function to print a list of integers
fun printList(list: List<Int>) {
    for (i in list) {
        print("$i ")
    }
    println()
}

// Get user input for the list of integers
val input = getUserInput("Enter a list of integers, separated by spaces: ")

// Convert the input string to a list of integers
val list = Arrays.stream(input.split(" ").toTypedArray()).mapToInt { it.toInt() }.boxed()
    .toList()

// Find the minimum, maximum, sum, average, median, mode, range, variance, and standard deviation of the list
val minimum = findMinimum(list)
val maximum = findMaximum(list)
val sum = findSum(list)
val average = findAverage(list)
val median = findMedian(list)
val mode = findMode(list)
val range = findRange(list)
val variance = findVariance(list)
val standardDeviation = findStandardDeviation(list)

// Print the results
println("Minimum: $minimum")
println("Maximum: $maximum")
println("Sum: $sum")
println("Average: $average")
println("Median: $median")
println("Mode: $mode")
println("Range: $range")
println("Variance: $variance")
println("Standard Deviation: $standardDeviation")
```

This code is a complex and differentiated Kotlin program that performs a variety of statistical calculations on a list of integers. It includes functions to find the minimum, maximum, sum, average, median, mode, range, variance, and standard deviation of a list of integers. The program also includes a function to get user input and a function to print a list of integers.