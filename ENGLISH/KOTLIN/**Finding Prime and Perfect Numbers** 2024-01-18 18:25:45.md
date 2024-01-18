```kotlin
// Import necessary libraries
import java.util.Scanner
import java.util.Arrays
import java.util.HashMap
import java.util.ArrayList

// Define the main function
fun main(args: Array<String>) {
    // Create a Scanner object to read input from the console
    val scanner = Scanner(System.`in`)

    // Prompt the user to enter an integer
    print("Enter an integer: ")

    // Read the integer from the console
    val n = scanner.nextInt()

    // Create an array of integers to store the prime numbers
    val primes = IntArray(n)

    // Initialize the array with -1 to indicate that the numbers have not been processed yet
    Arrays.fill(primes, -1)

    // Initialize the first two prime numbers (2 and 3)
    primes[0] = 2
    primes[1] = 3

    // Initialize a counter to keep track of the number of prime numbers found
    var count = 2

    // Use a loop to find all the prime numbers up to n
    for (i in 4..n) {
        // Assume that the number is prime initially
        var isPrime = true

        // Check if the number is divisible by any of the prime numbers found so far
        for (j in 0 until count) {
            if (i % primes[j] == 0) {
                // If the number is divisible by a prime number, it is not prime
                isPrime = false
                break
            }
        }

        // If the number is still prime after checking all the prime numbers found so far,
        // add it to the array of primes and increment the counter
        if (isPrime) {
            primes[count++] = i
        }
    }

    // Print the prime numbers found
    println("Prime numbers up to $n:")
    for (i in 0 until count) {
        print("${primes[i]} ")
    }
    println()

    // Now, let's find all the perfect numbers up to n

    // Create a HashMap to store the factors of each number
    val factorsMap = HashMap<Int, ArrayList<Int>>()

    // Use a loop to find the factors of each number up to n
    for (i in 1..n) {
        // Create an ArrayList to store the factors of the number
        val factors = ArrayList<Int>()

        // Find all the factors of the number
        for (j in 1..i / 2) {
            if (i % j == 0) {
                factors.add(j)
            }
        }

        // Add the factors of the number to the HashMap
        factorsMap[i] = factors
    }

    // Create an ArrayList to store the perfect numbers found
    val perfectNumbers = ArrayList<Int>()

    // Use a loop to find all the perfect numbers up to n
    for (i in 1..n) {
        // Get the factors of the number
        val factors = factorsMap[i]!!

        // Calculate the sum of the factors
        var sum = 0
        for (factor in factors) {
            sum += factor
        }

        // If the sum of the factors is equal to the number itself, the number is perfect
        if (sum == i) {
            perfectNumbers.add(i)
        }
    }

    // Print the perfect numbers found
    println("Perfect numbers up to $n:")
    for (perfectNumber in perfectNumbers) {
        print("$perfectNumber ")
    }
    println()
}
```

**Explanation:**

This code is a complex and differentiated program written in Kotlin that finds and prints all the prime numbers and perfect numbers up to a given integer n. The code uses a variety of advanced concepts such as arrays, loops, HashMaps, ArrayLists, and conditional statements.

* The program starts by prompting the user to enter an integer n.
* It then creates an array of integers called primes to store the prime numbers found.
* The array is initialized with -1 to indicate that the numbers have not been processed yet.
* The first two prime numbers (2 and 3) are initialized.
* A counter called count is initialized to keep track of the number of prime numbers found.
* The program uses a loop to find all the prime numbers up to n.
* For each number i from 4 to n, the program checks if it is prime.
* If the number is divisible by any of the prime numbers found so far, it is not prime.
* If the number is still prime after checking all the prime numbers found so far, it is added to the array of primes and the counter is incremented.
* After finding all the prime numbers, the program prints them out.
* The program then creates a HashMap called factorsMap to store the factors of each number up to n.
* The program uses a loop to find the factors of each number up to n.
* For each number i from 1 to n, the program finds all its factors and adds them to the HashMap.
* The program then creates an ArrayList called perfectNumbers to store the perfect numbers found.
* The program uses a loop to find all the perfect numbers up to n.
* For each number i from 1 to n, the program gets its factors from the HashMap and calculates the sum of the factors.
* If the sum of the factors is equal to the number itself, the number is perfect and is added to the ArrayList of perfect numbers.
* After finding all the perfect numbers, the program prints them out.

This code demonstrates the use of advanced programming concepts in Kotlin to solve a complex problem. It is a good example of how Kotlin can be used to write efficient and readable code.