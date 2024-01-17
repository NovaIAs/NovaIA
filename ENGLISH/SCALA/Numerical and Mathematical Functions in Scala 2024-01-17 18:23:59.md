```scala
// Define a type alias for a function that takes two integers and returns a Boolean
type IntPredicate = (Int, Int) => Boolean

// Define a function that checks if a given integer is even
def isEven(n: Int): Boolean = n % 2 == 0

// Define a function that checks if a given integer is odd
def isOdd(n: Int): Boolean = n % 2 != 0

// Define a function that checks if a given integer is positive
def isPositive(n: Int): Boolean = n > 0

// Define a function that checks if a given integer is negative
def isNegative(n: Int): Boolean = n < 0

// Define a function that checks if a given integer is zero
def isZero(n: Int): Boolean = n == 0

// Define a function that checks if a given integer is greater than another integer
def isGreaterThan(n: Int, m: Int): Boolean = n > m

// Define a function that checks if a given integer is less than another integer
def isLessThan(n: Int, m: Int): Boolean = n < m

// Define a function that checks if a given integer is greater than or equal to another integer
def isGreaterThanOrEqualTo(n: Int, m: Int): Boolean = n >= m

// Define a function that checks if a given integer is less than or equal to another integer
def isLessThanOrEqualTo(n: Int, m: Int): Boolean = n <= m

// Define a function that checks if a given integer is equal to another integer
def isEqual(n: Int, m: Int): Boolean = n == m

// Define a function that checks if a given integer is not equal to another integer
def isNotEqual(n: Int, m: Int): Boolean = n != m

// Define a function that checks if a given integer is a prime number
def isPrime(n: Int): Boolean = {
  if (n <= 1) {
    false
  } else if (n <= 3) {
    true
  } else if (n % 2 == 0 || n % 3 == 0) {
    false
  } else {
    var i = 5
    while (i * i <= n) {
      if (n % i == 0 || n % (i + 2) == 0) {
        return false
      }
      i += 6
    }
    true
  }
}

// Define a function that finds the greatest common divisor of two integers
def gcd(n: Int, m: Int): Int = {
  if (m == 0) {
    n
  } else {
    gcd(m, n % m)
  }
}

// Define a function that finds the least common multiple of two integers
def lcm(n: Int, m: Int): Int = {
  n * m / gcd(n, m)
}

// Define a function that finds the sum of the digits of a given integer
def sumDigits(n: Int): Int = {
  if (n == 0) {
    0
  } else {
    n % 10 + sumDigits(n / 10)
  }
}

// Define a function that finds the reverse of a given integer
def reverseDigits(n: Int): Int = {
  if (n == 0) {
    0
  } else {
    n % 10 * Math.pow(10, Math.log10(n.abs).toInt - 1) + reverseDigits(n / 10)
  }
}

// Define a function that finds the number of digits in a given integer
def countDigits(n: Int): Int = {
  if (n == 0) {
    0
  } else {
    1 + countDigits(n / 10)
  }
}

// Define a function that finds the factorial of a given integer
def factorial(n: Int): Int = {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}

// Define a function that finds the nth Fibonacci number
def fibonacci(n: Int): Int = {
  if (n == 0 || n == 1) {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}

// Define a function that finds the nth term of a geometric progression
def geometricProgression(a: Double, r: Double, n: Int): Double = {
  a * Math.pow(r, n - 1)
}

// Define a function that finds the sum of a geometric progression
def sumGeometricProgression(a: Double, r: Double, n: Int): Double = {
  a * (Math.pow(r, n) - 1) / (r - 1)
}

// Define a function that finds the nth term of an arithmetic progression
def arithmeticProgression(a: Int, d: Int, n: Int): Int = {
  a + (n - 1) * d
}

// Define a function that finds the sum of an arithmetic progression
def sumArithmeticProgression(a: Int, d: Int, n: Int): Int = {
  n * (2 * a + (n - 1) * d) / 2
}

// Define a function that finds the roots of a quadratic equation
def quadraticRoots(a: Double, b: Double, c: Double): (Double, Double) = {
  val discriminant = b * b - 4 * a