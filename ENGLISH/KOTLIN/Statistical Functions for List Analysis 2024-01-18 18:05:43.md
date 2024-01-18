```kotlin
// Define a function to calculate the factorial of a number
fun factorial(number: Int): Int {
    if (number == 0) {
        return 1
    } else {
        return number * factorial(number - 1)
    }
}

// Define a function to check if a number is prime
fun isPrime(number: Int): Boolean {
    if (number <= 1) {
        return false
    }

    for (i in 2..number / 2) {
        if (number % i == 0) {
            return false
        }
    }

    return true
}

// Define a function to generate a random list of numbers
fun generateRandomList(size: Int): List<Int> {
    val randomList = mutableListOf<Int>()

    for (i in 0 until size) {
        randomList.add((0..100).random())
    }

    return randomList
}

// Define a function to find the maximum and minimum values in a list
fun findMinMax(list: List<Int>): Pair<Int, Int> {
    var maxValue = list[0]
    var minValue = list[0]

    for (number in list) {
        if (number > maxValue) {
            maxValue = number
        }
        if (number < minValue) {
            minValue = number
        }
    }

    return Pair(maxValue, minValue)
}

// Define a function to sort a list in ascending order
fun sortAscending(list: List<Int>): List<Int> {
    return list.sorted()
}

// Define a function to sort a list in descending order
fun sortDescending(list: List<Int>): List<Int> {
    return list.sortedDescending()
}

// Define a function to find the median of a list
fun findMedian(list: List<Int>): Double {
    val sortedList = list.sorted()
    val middleIndex = sortedList.size / 2

    return if (sortedList.size % 2 == 0) {
        (sortedList[middleIndex] + sortedList[middleIndex - 1]) / 2.0
    } else {
        sortedList[middleIndex].toDouble()
    }
}

// Define a function to find the mode of a list
fun findMode(list: List<Int>): Int {
    val map = mutableMapOf<Int, Int>()

    for (number in list) {
        map[number] = map.getOrDefault(number, 0) + 1
    }

    var maxValue = 0
    var mode = 0

    for (entry in map) {
        if (entry.value > maxValue) {
            maxValue = entry.value
            mode = entry.key
        }
    }

    return mode
}

// Define a function to find the range of a list
fun findRange(list: List<Int>): Int {
    val maxValue = list.maxOrNull() ?: 0
    val minValue = list.minOrNull() ?: 0

    return maxValue - minValue
}

// Define a function to find the interquartile range of a list
fun findInterquartileRange(list: List<Int>): Double {
    val sortedList = list.sorted()
    val q1 = sortedList[sortedList.size / 4]
    val q3 = sortedList[sortedList.size * 3 / 4]

    return q3 - q1
}

// Define a function to find the standard deviation of a list
fun findStandardDeviation(list: List<Int>): Double {
    val mean = list.average()

    val squaredDifferences = mutableListOf<Double>()

    for (number in list) {
        squaredDifferences.add(Math.pow(number - mean, 2.0))
    }

    val variance = squaredDifferences.sum() / list.size

    return Math.sqrt(variance)
}

// Define a function to find the covariance of two lists
fun findCovariance(list1: List<Int>, list2: List<Int>): Double {
    if (list1.size != list2.size) {
        throw IllegalArgumentException("Lists must be of the same size")
    }

    val mean1 = list1.average()
    val mean2 = list2.average()

    val products = mutableListOf<Double>()

    for (i in list1.indices) {
        products.add((list1[i] - mean1) * (list2[i] - mean2))
    }

    return products.sum() / list1.size
}

// Define a function to find the correlation coefficient of two lists
fun findCorrelationCoefficient(list1: List<Int>, list2: List<Int>): Double {
    val covariance = findCovariance(list1, list2)
    val standardDeviation1 = findStandardDeviation(list1)
    val standardDeviation2 = findStandardDeviation(list2)

    return covariance / (standardDeviation1 * standardDeviation2)
}
```

This code provides various statistical functions that can be used to analyze a list of numbers. It includes functions for calculating the factorial of a number, checking if a number is prime, generating a random list of numbers, finding the maximum and minimum values in a list, sorting a list in ascending and descending order, finding the median, mode, range, interquartile range, standard deviation, covariance, and correlation coefficient of a list. These functions can be useful for various statistical analyses and data manipulations.