```scala
object ComplexScalaCode {

  // Define a higher-order function that takes a list of integers and a function that operates
  // on each integer in the list and returns a new list of integers
  def map(list: List[Int], f: Int => Int): List[Int] = {
    list match {
      case Nil => Nil
      case x :: xs => f(x) :: map(xs, f)
    }
  }

  // Define a function that calculates the factorial of an integer using recursion
  def factorial(n: Int): BigInt = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  // Define a function that calculates the nth Fibonacci number using recursion
  def fibonacci(n: Int): BigInt = {
    if (n <= 1) n
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  // Define a function that checks if a given number is prime using the Miller-Rabin primality test
  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else {
      // Check if n is divisible by any number from 2 to sqrt(n)
      var i = 2
      while (i * i <= n) {
        if (n % i == 0) return false
        i += 1
      }
      true
    }
  }

  // Define a function that generates a list of all prime numbers up to a given number using the
  // Sieve of Eratosthenes algorithm
  def primes(n: Int): List[Int] = {
    val sieve = Array.fill(n + 1)(true)
    sieve(0) = false
    sieve(1) = false

    var i = 2
    while (i * i <= n) {
      if (sieve(i)) {
        var j = i * i
        while (j <= n) {
          sieve(j) = false
          j += i
        }
      }
      i += 1
    }

    sieve.zipWithIndex.filter(_._1).map(_._2)
  }

  // Define a function that calculates the greatest common divisor of two integers using the
  // Euclidean algorithm
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  // Define a function that calculates the least common multiple of two integers using the
  // formula lcm(a, b) = a * b / gcd(a, b)
  def lcm(a: Int, b: Int): Int = {
    a * b / gcd(a, b)
  }

  // Define a function that calculates the sum of the digits of a given integer using recursion
  def sumDigits(n: Int): Int = {
    if (n < 10) n
    else sumDigits(n / 10) + (n % 10)
  }

  // Define a function that checks if a given string is a palindrome using recursion
  def isPalindrome(s: String): Boolean = {
    if (s.length <= 1) true
    else s.head == s.last && isPalindrome(s.substring(1, s.length - 1))
  }

  // Define a function that reverses a given list using recursion
  def reverseList[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => Nil
      case x :: xs => reverseList(xs) ::: List(x)
    }
  }

  // Define a function that merges two sorted lists into a single sorted list using recursion
  def mergeLists[A](xs: List[A], ys: List[A])(implicit ord: Ordering[A]): List[A] = {
    (xs, ys) match {
      case (Nil, _) => ys
      case (_, Nil) => xs
      case (x :: xs1, y :: ys1) =>
        if (ord.lt(x, y)) x :: mergeLists(xs1, ys)
        else y :: mergeLists(xs, ys1)
    }
  }

  // Define a function that sorts a list of integers using the quicks