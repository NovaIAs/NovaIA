```groovy
// Define a closure to calculate the factorial of a number
def factorial = { number ->
    if (number == 0) {
        1
    } else {
        number * factorial(number - 1)
    }
}

// Define a closure to calculate the Fibonacci sequence
def fibonacci = { number ->
    if (number <= 1) {
        number
    } else {
        fibonacci(number - 1) + fibonacci(number - 2)
    }
}

// Define a closure to check if a number is prime
def isPrime = { number ->
    if (number <= 1) {
        false
    } else if (number == 2) {
        true
    } else {
        !(2..(number / 2)).any { number % it == 0 }
    }
}

// Create a list of numbers from 1 to 100
def numbers = (1..100).toList()

// Print the factorials of the numbers
println "Factorials:"
numbers.each { println it + "! = " + factorial(it) }

// Print the Fibonacci sequence
println "Fibonacci Sequence:"
numbers.take(20).each { println it + ": " + fibonacci(it) }

// Print the prime numbers
println "Prime Numbers:"
numbers.findAll { isPrime(it) }.each { println it }
```

This code demonstrates the use of closures in Groovy to define complex and reusable blocks of code. The closures are used to calculate the factorial of a number, the Fibonacci sequence, and check if a number is prime. The code then uses these closures to print the factorials of numbers from 1 to 100, the first 20 numbers of the Fibonacci sequence, and all the prime numbers from 1 to 100.

Here's a breakdown of the code:

1. Define Closures:
   - `factorial`: A closure that calculates the factorial of a number using recursion.
   - `fibonacci`: A closure that calculates the nth number of the Fibonacci sequence.
   - `isPrime`: A closure that checks if a number is prime.

2. Generate a List of Numbers:
   - `numbers`: A list of numbers from 1 to 100 is created using the `(1..100).toList()` expression.

3. Print Factorials:
   - The `each` method is used to iterate over the `numbers` list and print the factorial of each number using the `factorial` closure.

4. Print Fibonacci Sequence:
   - The `take(20)` method is used to get the first 20 numbers from the `numbers` list. Then, the `each` method is used to iterate over these numbers and print the Fibonacci number for each using the `fibonacci` closure.

5. Print Prime Numbers:
   - The `findAll` method is used to filter the `numbers` list and select only the prime numbers using the `isPrime` closure. Then, the `each` method is used to iterate over these prime numbers and print each one.