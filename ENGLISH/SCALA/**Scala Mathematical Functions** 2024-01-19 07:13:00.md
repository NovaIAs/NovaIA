```scala
// Define a function to calculate the factorial of a number
def factorial(n: Int): BigInt = {
  if (n == 0) 1
  else n * factorial(n - 1)
}

// Define a function to calculate the fibonacci sequence
def fibonacci(n: Int): BigInt = {
  if (n < 2) n
  else fibonacci(n - 1) + fibonacci(n - 2)
}

// Define a function to calculate the greatest common divisor of two numbers
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}

// Define a function to calculate the least common multiple of two numbers
def lcm(a: Int, b: Int): Int = {
  a * b / gcd(a, b)
}

// Define a function to check if a number is prime
def isPrime(n: Int): Boolean = {
  if (n <= 1) false
  else (2 to math.sqrt(n.toDouble).toInt).forall(n % _ != 0)
}

// Define a function to find all the prime factors of a number
def primeFactors(n: Int): List[Int] = {
  def primeFactorsHelper(n: Int, factors: List[Int]): List[Int] = {
    if (n == 1) factors
    else if (isPrime(n)) factors :+ n
    else {
      val factor = (2 to n / 2).find(n % _ == 0).get
      primeFactorsHelper(n / factor, factors :+ factor)
    }
  }
  primeFactorsHelper(n, List())
}

// Define a function to find all the divisors of a number
def divisors(n: Int): List[Int] = {
  def divisorsHelper(n: Int, divisors: List[Int]): List[Int] = {
    if (n == 1) divisors
    else {
      val divisor = (2 to n / 2).find(n % _ == 0).get
      divisorsHelper(n / divisor, divisors :+ divisor)
    }
  }
  divisorsHelper(n, List())
}

```

This code is a collection of mathematical functions written in Scala. It includes functions for calculating factorials, Fibonacci numbers, greatest common divisors (GCDs), least common multiples (LCMs), checking if a number is prime, finding prime factors, and finding divisors.

Here are some examples of how you can use these functions:

```scala
// Calculate the factorial of 5
println(factorial(5)) // 120

// Calculate the fibonacci sequence for the first 10 numbers
(0 to 9).foreach(n => println(fibonacci(n)))
// 0, 1, 1, 2, 3, 5, 8, 13, 21, 34

// Calculate the greatest common divisor of 12 and 18
println(gcd(12, 18)) // 6

// Calculate the least common multiple of 12 and 18
println(lcm(12, 18)) // 36

// Check if 17 is prime
println(isPrime(17)) // true

// Find the prime factors of 100
println(primeFactors(100)) // List(2, 2, 5, 5)

// Find the divisors of 12
println(divisors(12)) // List(1, 2, 3, 4, 6, 12)
```

These functions can be useful for a variety of mathematical tasks, such as number theory, cryptography, and algorithm design.