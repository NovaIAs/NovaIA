```kotlin
// This is a function that takes two integers as parameters and returns their sum.
fun add(a: Int, b: Int): Int {
    return a + b
}

// This is a function that takes a list of integers as a parameter and returns the sum of all the integers in the list.
fun sum(numbers: List<Int>): Int {
    var total = 0
    for (number in numbers) {
        total += number
    }
    return total
}

// This is a function that takes a list of integers as a parameter and returns the average of all the integers in the list.
fun average(numbers: List<Int>): Double {
    val total = sum(numbers)
    return total / numbers.size.toDouble()
}

// This is a function that takes a list of integers as a parameter and returns the maximum value in the list.
fun max(numbers: List<Int>): Int {
    var max = Int.MIN_VALUE
    for (number in numbers) {
        if (number > max) {
            max = number
        }
    }
    return max
}

// This is a function that takes a list of integers as a parameter and returns the minimum value in the list.
fun min(numbers: List<Int>): Int {
    var min = Int.MAX_VALUE
    for (number in numbers) {
        if (number < min) {
            min = number
        }
    }
    return min
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the even numbers from the original list.
fun evenNumbers(numbers: List<Int>): List<Int> {
    val evenNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (number % 2 == 0) {
            evenNumbers.add(number)
        }
    }
    return evenNumbers
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the odd numbers from the original list.
fun oddNumbers(numbers: List<Int>): List<Int> {
    val oddNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (number % 2 != 0) {
            oddNumbers.add(number)
        }
    }
    return oddNumbers
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the prime numbers from the original list.
fun primeNumbers(numbers: List<Int>): List<Int> {
    val primeNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (isPrime(number)) {
            primeNumbers.add(number)
        }
    }
    return primeNumbers
}

// This is a function that checks if a given number is prime.
fun isPrime(number: Int): Boolean {
    if (number <= 1) {
        return false
    }
    for (i in 2 until number) {
        if (number % i == 0) {
            return false
        }
    }
    return true
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the composite numbers from the original list.
fun compositeNumbers(numbers: List<Int>): List<Int> {
    val compositeNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (!isPrime(number)) {
            compositeNumbers.add(number)
        }
    }
    return compositeNumbers
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the perfect numbers from the original list.
fun perfectNumbers(numbers: List<Int>): List<Int> {
    val perfectNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (isPerfect(number)) {
            perfectNumbers.add(number)
        }
    }
    return perfectNumbers
}

// This is a function that checks if a given number is perfect.
fun isPerfect(number: Int): Boolean {
    if (number <= 1) {
        return false
    }
    var divisors = mutableListOf<Int>()
    for (i in 1 until number) {
        if (number % i == 0) {
            divisors.add(i)
        }
    }
    val sumOfDivisors = divisors.sum()
    return sumOfDivisors == number
}

// This is a function that takes a list of integers as a parameter and returns a new list containing only the abundant numbers from the original list.
fun abundantNumbers(numbers: List<Int>): List<Int> {
    val abundantNumbers = mutableListOf<Int>()
    for (number in numbers) {
        if (isAbundant(number)) {
            abundantNumbers.add(number)
        }
    }
    return abundantNumbers
}

// This is a function that checks if a given number is abundant.
fun isAbundant(number: Int): Boolean {
    if (number <= 1) {
        return false
    }
    var divisors = mutableListOf<Int>()
    for (i in 1 until number) {
        if (number % i == 0) {
            divisors.add(i)
        }
    }
    val sumOfDivisors = divisors