Certainly! Here's a complex and differentiated Kotlin code that demonstrates various programming concepts:

```kotlin
import java.util.*

class ComplexCode {
    private var numbers: MutableList<Int> = mutableListOf()
    
    fun generateNumbers(size: Int) {
        val random = Random()
        for (i in 0 until size) {
            numbers.add(random.nextInt(100))
        }
    }
    
    fun performSort() {
        numbers.sort()
    }
    
    fun findPrimeNumbers(): List<Int> {
        val primeNumbers: MutableList<Int> = mutableListOf()
        for (num in numbers) {
            if (isPrime(num)) {
                primeNumbers.add(num)
            }
        }
        return primeNumbers
    }
    
    private fun isPrime(number: Int): Boolean {
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
    
    fun printNumbers() {
        for (num in numbers) {
            println(num)
        }
    }
}

fun main() {
    val code = ComplexCode()
    code.generateNumbers(10)
    println("Generated Numbers:")
    code.printNumbers()
    
    code.performSort()
    println("Sorted Numbers:")
    code.printNumbers()
    
    val primeNumbers = code.findPrimeNumbers()
    println("Prime Numbers:")
    for (prime in primeNumbers) {
        println(prime)
    }
}
```

Explanation:

This code demonstrates several programming concepts such as classes, lists, loops, conditionals, and functions in Kotlin.

The `ComplexCode` class represents a complex code structure. It has a private mutable list of integers called `numbers`, which is used to store randomly generated numbers.

The `generateNumbers` function generates random numbers using the `java.util.Random` class. It takes a parameter `size` indicating the number of random numbers to generate.

The `performSort` function sorts the generated numbers in ascending order using the `sort` method provided by the `MutableList` class.

The `findPrimeNumbers` function iterates over the generated numbers and checks if each number is prime using the `isPrime` helper function. Prime numbers are stored in a new list called `primeNumbers`, which is then returned.

The `isPrime` function checks whether a given number is prime or not. It uses a simple algorithm to check divisibility by numbers from 2 to `number - 1`.

The `printNumbers` function simply prints each number in the `numbers` list.

In the `main` function, an instance of `ComplexCode` is created, and the `generateNumbers` function is called to generate 10 random numbers. These numbers are then printed.

The `performSort` function is called to sort the numbers, and the sorted numbers are printed.

Finally, the `findPrimeNumbers` function is called to find and print all the prime numbers from the generated list.