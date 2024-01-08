```groovy

// Create a list of numbers from 1 to 100
def numbers = (1..100).toList()

// Create a map of numbers to their squares
def squares = numbers.collectEntries { [it, it * it] }

// Create a list of even numbers from 1 to 100
def evenNumbers = numbers.findAll { it % 2 == 0 }

// Create a map of even numbers to their squares
def evenSquares = evenNumbers.collectEntries { [it, it * it] }

// Print the list of numbers
println "Numbers: ${numbers}"

// Print the map of numbers to their squares
println "Squares: ${squares}"

// Print the list of even numbers
println "Even numbers: ${evenNumbers}"

// Print the map of even numbers to their squares
println "Even squares: ${evenSquares}"

// Create a closure that takes a number and returns its square
def squareClosure = { number -> number * number }

// Create a list of numbers from 1 to 100 and apply the square closure to each number
def squaredNumbers = numbers.collect(squareClosure)

// Print the list of squared numbers
println "Squared numbers: ${squaredNumbers}"

// Create a list of even numbers from 1 to 100 using the findEvenClosure
def evenNumbers2 = numbers.findAll { findEvenClosure(it) }

// Print the list of even numbers
println "Even numbers (using closure): ${evenNumbers2}"

// Define a closure that takes a number and returns true if it is even, false otherwise
def findEvenClosure = { number -> number % 2 == 0 }
```

This code is a complex and differentiated code in Groovy. It includes a variety of features, including:

* The use of list comprehensions to create lists and maps
* The use of closures to define anonymous functions
* The use of the `collect` method to apply a closure to each element of a list
* The use of the `findAll` method to filter a list based on a closure
* The use of the `println` method to print output to the console

The code is also well-commented, which makes it easy to understand what each part of the code is doing.

Here is a brief explanation of the code:

* The first part of the code creates a list of numbers from 1 to 100.
* The second part of the code creates a map of numbers to their squares.
* The third part of the code creates a list of even numbers from 1 to 100.
* The fourth part of the code creates a map of even numbers to their squares.
* The fifth part of the code prints the list of numbers, the map of numbers to their squares, the list of even numbers, and the map of even numbers to their squares.
* The sixth part of the code creates a closure that takes a number and returns its square.
* The seventh part of the code creates a list of numbers from 1 to 100 and applies the square closure to each number.
* The eighth part of the code prints the list of squared numbers.
* The ninth part of the code creates a list of even numbers from 1 to 100 using the `findEvenClosure`.
* The tenth part of the code prints the list of even numbers.

This code is a good example of how Groovy can be used to write complex and differentiated code in a concise and readable manner.