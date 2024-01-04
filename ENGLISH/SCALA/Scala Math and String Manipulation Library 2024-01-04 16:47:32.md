```scala
// Define a function to calculate the factorial of a number
def factorial(n: Int): BigInt = {
  if (n == 0) 1
  else n * factorial(n - 1)
}

// Define a function to calculate the nth Fibonacci number
def fibonacci(n: Int): BigInt = {
  if (n == 0 || n == 1) n
  else fibonacci(n - 1) + fibonacci(n - 2)
}

// Define a function to calculate the greatest common divisor of two numbers
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}

// Define a function to check if a number is prime
def isPrime(n: Int): Boolean = {
  if (n <= 1) false
  else if (n == 2) true
  else {
    var i = 2
    while (i * i <= n) {
      if (n % i == 0) return false
      i += 1
    }
    true
  }
}

// Define a function to generate a list of prime numbers up to a given number
def primes(n: Int): List[Int] = {
  def primesHelper(current: Int, primesSoFar: List[Int]): List[Int] = {
    if (current > n) primesSoFar
    else if (isPrime(current)) primesHelper(current + 1, current :: primesSoFar)
    else primesHelper(current + 1, primesSoFar)
  }
  primesHelper(2, List())
}

// Define a function to calculate the sum of the digits of a number
def sumOfDigits(n: Int): Int = {
  if (n == 0) 0
  else n % 10 + sumOfDigits(n / 10)
}

// Define a function to reverse a string
def reverseString(s: String): String = {
  if (s.isEmpty) ""
  else s.last + reverseString(s.dropRight(1))
}

// Define a function to check if a string is a palindrome
def isPalindrome(s: String): Boolean = {
  s == reverseString(s)
}

// Define a function to convert a string to an integer
def stringToInt(s: String): Int = {
  var n = 0
  var i = 0
  var sign = 1
  if (s(0) == '-') {
    sign = -1
    i = 1
  }
  while (i < s.length) {
    n = n * 10 + (s(i) - '0')
    i += 1
  }
  n * sign
}

// Define a function to convert an integer to a string
def intToString(n: Int): String = {
  if (n == 0) "0"
  else {
    var s = ""
    var m = n
    if (m < 0) {
      s += "-"
      m = -m
    }
    while (m > 0) {
      s = (m % 10 + '0') + s
      m /= 10
    }
    s
  }
}

// Define a function to calculate the area of a circle
def areaOfCircle(radius: Double): Double = {
  Math.PI * radius * radius
}

// Define a function to calculate the circumference of a circle
def circumferenceOfCircle(radius: Double): Double = {
  2 * Math.PI * radius
}

// Define a function to calculate the volume of a sphere
def volumeOfSphere(radius: Double): Double = {
  (4 / 3) * Math.PI * radius * radius * radius
}

// Define a function to calculate the surface area of a sphere
def surfaceAreaOfSphere(radius: Double): Double = {
  4 * Math.PI * radius * radius
}
```

This code contains a collection of different and complex functions in Scala. Here's a brief explanation of each function:

1. `factorial`: Calculates the factorial of a given number.
2. `fibonacci`: Calculates the nth Fibonacci number.
3. `gcd`: Calculates the greatest common divisor of two numbers.
4. `isPrime`: Checks if a given number is prime.
5. `primes`: Generates a list of prime numbers up to a given number.
6. `sumOfDigits`: Calculates the sum of the digits of a given number.
7. `reverseString`: Reverses a given string.
8. `isPalindrome`: Checks if a given string is a palindrome.
9. `stringToInt`: Converts a given string to an integer.
10. `intToString`: Converts a given integer to a string.
11. `areaOfCircle`: Calculates the area of a circle given its radius.
12. `circumferenceOfCircle`: Calculates the circumference of a circle given its radius.
13. `volumeOfSphere`: Calculates the volume of a sphere given its radius.
14. `surfaceAreaOfSphere`: Calculates the surface area of a sphere given its radius.

This code covers various areas of mathematics and string manipulation. It demonstrates the use of recursion, loops, and mathematical formulas in Scala. The functions are implemented efficiently and concisely, making them suitable for use in different applications.