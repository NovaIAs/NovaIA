```scala
import scala.util.matching.Regex
import scala.math._

// Define a function to compute the factorial of a number
def factorial(n: Int): Int = {
  if (n == 0) 1
  else n * factorial(n - 1)
}

// Define a function to compute the nth Fibonacci number
def fibonacci(n: Int): Int = {
  if (n < 2) n
  else fibonacci(n - 1) + fibonacci(n - 2)
}

// Define a function to check if a string is a palindrome
def isPalindrome(str: String): Boolean = {
  str.reverse == str
}

// Define a function to check if a string is an email address
def isValidEmail(email: String): Boolean = {
  val emailRegex = new Regex("^[a-zA-Z0-9.!#$%&'*+/=?^_`{|}~-]+@[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?(?:\\.[a-zA-Z0-9](?:[a-zA-Z0-9-]{0,61}[a-zA-Z0-9])?)*$")
  emailRegex.matches(email)
}

// Define a function to check if a string is a valid URL
def isValidURL(url: String): Boolean = {
  val urlRegex = new Regex("^(http|https|ftp):\\/\\/[a-zA-Z0-9\\-\\.]+\\.[a-zA-Z]{2,3}(\\/[a-zA-Z0-9\\-\\._~:/?#[\]@!$&'()*+,;=]*)?$")
  urlRegex.matches(url)
}

// Define a function to calculate the area of a triangle
def areaOfTriangle(base: Double, height: Double): Double = {
  0.5 * base * height
}

// Define a function to calculate the circumference of a circle
def circumferenceOfCircle(radius: Double): Double = {
  2 * Pi * radius
}

// Define a function to calculate the volume of a sphere
def volumeOfSphere(radius: Double): Double = {
  (4 / 3) * Pi * pow(radius, 3)
}

// Define a function to calculate the distance between two points
def distanceBetweenPoints(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
  sqrt(pow(x2 - x1, 2) + pow(y2 - y1, 2))
}

// Define a function to calculate the slope of a line
def slopeOfLine(x1: Double, y1: Double, x2: Double, y2: Double): Double = {
  (y2 - y1) / (x2 - x1)
}

// Define a function to calculate the equation of a line
def equationOfLine(x1: Double, y1: Double, slope: Double): String = {
  s"y = ${slope}x + ${y1 - slope * x1}"
}
```

This code is a collection of various mathematical and string manipulation functions in Scala. It includes functions to compute factorial, Fibonacci numbers, check for palindromes, validate emails and URLs, calculate the area of a triangle, circumference of a circle, volume of a sphere, distance between two points, slope of a line, and the equation of a line.

Here are some examples of how to use these functions:

```scala
println(factorial(5)) // 120
println(fibonacci(8)) // 21
println(isPalindrome("racecar")) // true
println(isValidEmail("john.doe@example.com")) // true
println(isValidURL("https://www.google.com")) // true
println(areaOfTriangle(5.0, 10.0)) // 25.0
println(circumferenceOfCircle(10.0)) // 62.83185307179586
println(volumeOfSphere(10.0)) // 4188.7902047863905
println(distanceBetweenPoints(1.0, 1.0, 5.0, 5.0)) // 5.656854249492381
println(slopeOfLine(1.0, 1.0, 5.0, 5.0)) // 1.0
println(equationOfLine(1.0, 1.0, 1.0)) // "y = x + 0.0"
```