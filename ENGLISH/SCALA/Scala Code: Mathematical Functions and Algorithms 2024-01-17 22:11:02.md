```scala
object ComplexScalaCode {

  // Define a higher-order function to calculate the factorial of a number
  def factorial(n: Int): Int = {
    def factorialHelper(n: Int, acc: Int): Int = {
      if (n == 0) acc
      else factorialHelper(n - 1, n * acc)
    }
    factorialHelper(n, 1)
  }

  // Define a function to generate a list of prime numbers up to a given limit
  def primesUpTo(limit: Int): List[Int] = {
    def isPrime(n: Int): Boolean = {
      if (n <= 1) false
      else {
        var i = 2
        while (i * i <= n) {
          if (n % i == 0) return false
          i += 1
        }
        true
      }
    }

    def primesHelper(n: Int, acc: List[Int]): List[Int] = {
      if (isPrime(n)) {
        if (n <= limit) primesHelper(n + 1, n :: acc)
        else acc
      } else primesHelper(n + 1, acc)
    }

    primesHelper(2, Nil)
  }

  // Define a function to calculate the Fibonacci sequence up to a given limit
  def fibonacciUpTo(limit: Int): List[Int] = {
    def fibonacciHelper(n: Int, acc: List[Int]): List[Int] = {
      if (n <= limit) {
        if (acc.length < 2) fibonacciHelper(n + 1, n :: acc)
        else fibonacciHelper(n + 1, (acc(0) + acc(1)) :: acc)
      } else acc
    }

    fibonacciHelper(0, Nil)
  }

  // Define a function to generate a list of tuples containing the factors of a given number
  def factors(n: Int): List[(Int, Int)] = {
    def factorsHelper(n: Int, acc: List[(Int, Int)]): List[(Int, Int)] = {
      var i = 2
      while (i * i <= n) {
        if (n % i == 0) {
          if (acc.exists(_ == (i, n / i))) factorsHelper(n / i, acc)
          else factorsHelper(n / i, (i, n / i) :: acc)
        }
        i += 1
      }
      if (i * i > n) (n, 1) :: acc
      else acc
    }

    factorsHelper(n, Nil)
  }

  // Define a function to calculate the greatest common divisor (GCD) of two numbers
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  // Define a function to calculate the least common multiple (LCM) of two numbers
  def lcm(a: Int, b: Int): Int = {
    if (a == 0 || b == 0) 0
    else (a * b) / gcd(a, b)
  }

  // Define a function to check if a number is a palindrome
  def isPalindrome(n: Int): Boolean = {
    val str = n.toString
    str == str.reverse
  }

  // Define a function to generate a list of all permutations of a given string
  def permutations(str: String): List[String] = {
    def permutationsHelper(str: String, acc: List[String]): List[String] = {
      if (str.length == 1) str :: acc
      else {
        var newAcc = List[String]()
        for (i <- 0 until str.length) {
          val newStr = str.substring(0, i) + str.substring(i + 1)
          newAcc ++= permutationsHelper(newStr, str(i).toString :: acc)
        }
        newAcc
      }
    }

    permutationsHelper(str, Nil)
  }

  // Define a function to generate a list of all combinations of a given string
  def combinations(str: String): List[String] = {
    def combinationsHelper(str: String, acc: List[String]): List[String] = {
      if (str.length == 1) str :: acc
      else {
        var newAcc = List[String]()
        for (i <- 0 until str.length) {
          val newStr = str.substring(0, i) + str.substring(i + 1