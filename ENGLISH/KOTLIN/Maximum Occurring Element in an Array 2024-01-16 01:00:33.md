```kotlin
// Kotlin code to find the maximum occurring element in an array

// Function to find the maximum occurring element in an array
fun findMaxOccurringElement(arr: Array<Int>): Int {
    // Create a HashMap to store the frequency of each element
    val frequencyMap = HashMap<Int, Int>()

    // Traverse the array and update the frequency of each element
    for (i in arr.indices) {
        val element = arr[i]
        val frequency = frequencyMap.getOrDefault(element, 0)
        frequencyMap[element] = frequency + 1
    }

    // Find the maximum frequency
    var maxFrequency = 0
    for (frequency in frequencyMap.values) {
        if (frequency > maxFrequency) {
            maxFrequency = frequency
        }
    }

    // Find the element with the maximum frequency
    var maxOccurringElement = -1
    for (element in frequencyMap.keys) {
        val frequency = frequencyMap[element]!!
        if (frequency == maxFrequency) {
            maxOccurringElement = element
        }
    }

    return maxOccurringElement
}

// Driver code
fun main(args: Array<String>) {
    val arr = intArrayOf(1, 2, 3, 4, 1, 2, 3)
    val maxOccurringElement = findMaxOccurringElement(arr)
    println("The maximum occurring element is: $maxOccurringElement")
}
```

Explanation:

1. We define a function called findMaxOccurringElement that takes an array of integers as input and returns the maximum occurring element in the array.

2. Inside the function, we create a HashMap called frequencyMap to store the frequency of each element in the array. The HashMap uses the element as the key and the frequency as the value.

3. We traverse the array using a for loop and update the frequency of each element in the frequencyMap.

4. We find the maximum frequency by iterating over the values in the frequencyMap and keeping track of the maximum frequency.

5. We find the element with the maximum frequency by iterating over the keys in the frequencyMap and checking if the frequency of the element is equal to the maximum frequency.

6. Finally, we return the maximum occurring element.

7. In the main function, we create an array of integers and call the findMaxOccurringElement function to find the maximum occurring element in the array.

8. We print the maximum occurring element to the console.