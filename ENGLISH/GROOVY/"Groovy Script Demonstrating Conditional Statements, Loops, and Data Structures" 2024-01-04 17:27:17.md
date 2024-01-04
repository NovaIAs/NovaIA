```groovy
// This Groovy script demonstrates the use of conditional statements, loops, and data structures.

// Define a list of numbers
def numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

// Iterate over the list using a for loop
for (number in numbers) {
    // Check if the number is even or odd using a conditional statement
    if (number % 2 == 0) {
        // If the number is even, print "Even"
        println "${number} is Even"
    } else {
        // If the number is odd, print "Odd"
        println "${number} is Odd"
    }
}

// Define a map to store key-value pairs
def fruits = [
    apple: "red",
    banana: "yellow",
    orange: "orange",
    grapes: "purple"
]

// Iterate over the map using a for loop
for (fruit in fruits) {
    // Print the key and value using a string interpolation
    println "${fruit.key} is ${fruit.value}"
}

// Define a closure to calculate the factorial of a number
def factorial = { n ->
    if (n == 0) {
        1
    } else {
        n * factorial(n - 1)
    }
}

// Calculate the factorial of 5 using the closure
def result = factorial(5)

// Print the result
println "Factorial of 5 is ${result}"
```
Explanation:

1. Define a list of numbers using the `def` keyword and square brackets `[]`.

2. Iterate over the list using a `for` loop with the `in` keyword.

3. Inside the loop, use a conditional statement (`if-else`) to check if the number is even or odd.

4. Print the result using `println`.

5. Define a map to store key-value pairs using the `def` keyword and square brackets `[]`.

6. Iterate over the map using a `for` loop with the `in` keyword.

7. Inside the loop, use string interpolation to print the key and value.

8. Define a closure to calculate the factorial of a number. A closure is a block of code that can be passed around and executed later.

9. Calculate the factorial of 5 using the closure.

10. Print the result.