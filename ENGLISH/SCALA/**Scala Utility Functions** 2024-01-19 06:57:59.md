```scala
// Define a function to calculate the factorial of a number
def factorial(n: Int): Int = {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}

// Define a function to check if a number is prime
def isPrime(n: Int): Boolean = {
  if (n <= 1) {
    false
  } else if (n == 2) {
    true
  } else {
    !(2 to math.sqrt(n).toInt).exists(i => n % i == 0)
  }
}

// Define a function to calculate the sum of the digits of a number
def sumOfDigits(n: Int): Int = {
  if (n < 10) {
    n
  } else {
    n % 10 + sumOfDigits(n / 10)
  }
}

// Define a function to reverse a string
def reverseString(s: String): String = {
  if (s.isEmpty) {
    ""
  } else {
    reverseString(s.tail) + s.head
  }
}

// Define a function to generate a random number between 1 and 100
def randomInt(min: Int, max: Int): Int = {
  val random = new scala.util.Random
  random.nextInt(max - min + 1) + min
}

// Main function
object Main {
  def main(args: Array[String]): Unit = {
    // Print the factorial of 5
    println(factorial(5))

    // Print if the number 7 is prime
    println(isPrime(7))

    // Print the sum of the digits of the number 12345
    println(sumOfDigits(12345))

    // Print the reversed string of "Hello"
    println(reverseString("Hello"))

    // Print a random number between 1 and 100
    println(randomInt(1, 100))
  }
}
```

This code contains several functions that perform various tasks:

1. `factorial`: Calculates the factorial of a given number using recursion.
2. `isPrime`: Checks if a given number is prime by testing divisibility from 2 up to the square root of the number.
3. `sumOfDigits`: Computes the sum of the digits of a given number by recursively extracting the last digit and adding it to the sum of the remaining digits.
4. `reverseString`: Reverses a given string using recursion by appending the last character to the reversed substring.
5. `randomInt`: Generates a random integer between a specified minimum and maximum value.

The `Main` object contains the `main` function, which is the entry point of the program. In the `main` function, we call each of the defined functions and print the results.

This code demonstrates the use of various Scala features, including functions, recursion, control flow, and object-oriented programming.