```scala
object ComplexScalaCode {

  // Define a higher-order function that takes a function as an argument
  def withFilter(f: Int => Boolean)(xs: List[Int]): List[Int] = {
    xs.filter(f)
  }

  // Define a function that checks if a number is even
  def isEven(x: Int): Boolean = {
    x % 2 == 0
  }

  // Define a function that checks if a number is prime
  def isPrime(x: Int): Boolean = {
    (2 to math.sqrt(x).toInt).forall(x % _ != 0)
  }

  // Define a function that returns the first n prime numbers
  def firstPrimes(n: Int): List[Int] = {
    Stream.from(2).filter(isPrime).take(n).toList
  }

  // Define a function that returns the Fibonacci sequence up to n
  def fibonacci(n: Int): List[Int] = {
    def fib(n: Int): Int = {
      if (n <= 1) n
      else fib(n-1) + fib(n-2)
    }
    (0 to n).map(fib).toList
  }

  // Define a function that returns the factorial of a number
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n-1)
  }

  // Define a function that returns the greatest common divisor of two numbers
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  // Define a function that returns the least common multiple of two numbers
  def lcm(a: Int, b: Int): Int = {
    a * b / gcd(a, b)
  }

  // Define a function that returns the sum of the digits of a number
  def sumDigits(n: Int): Int = {
    if (n == 0) 0
    else n % 10 + sumDigits(n / 10)
  }

  // Define a function that returns the reverse of a number
  def reverseNumber(n: Int): Int = {
    if (n == 0) 0
    else n % 10 * math.pow(10, math.log10(n).toInt).toInt + reverseNumber(n / 10)
  }

  // Define a function that returns the number of digits in a number
  def numDigits(n: Int): Int = {
    if (n == 0) 0
    else 1 + numDigits(n / 10)
  }

  // Define a function that returns the nth Fibonacci number
  def fib(n: Int): Int = {
    if (n <= 1) n
    else fib(n-1) + fib(n-2)
  }

  // Define a function that returns the sum of the first n Fibonacci numbers
  def sumFib(n: Int): Int = {
    if (n <= 1) n
    else sumFib(n-1) + fib(n)
  }

  // Define a function that returns the sum of the first n prime numbers
  def sumPrimes(n: Int): Int = {
    firstPrimes(n).sum
  }

  // Define a function that returns the sum of the first n even numbers
  def sumEvens(n: Int): Int = {
    (1 to n).filter(isEven).sum
  }

  // Define a function that returns the sum of the first n odd numbers
  def sumOdds(n: Int): Int = {
    (1 to n).filter(!isEven(_)).sum
  }

  // Define a function that returns the product of the first n prime numbers
  def productPrimes(n: Int): Int = {
    firstPrimes(n).product
  }

  // Define a function that returns the product of the first n even numbers
  def productEvens(n: Int): Int = {
    (1 to n).filter(isEven).product
  }

  // Define a function that returns the product of the first n odd numbers
  def productOdds(n: Int): Int = {
    (1 to n).filter(!isEven(_)).product
  }

  // Define a function that returns the average of the first n prime numbers
  def avgPrimes(n: Int): Double = {
    firstPrimes(n).sum / n.toDouble
  }

  // Define a function that returns the average of the first n even numbers
  def avgEvens(n: Int): Double = {
    (1 to n).filter(isEven).sum / n.toDouble
  }

  // Define a function that returns the average of the first n odd numbers
  def avgOdds(n: Int): Double = {
    (1 to n).filter(!isEven(_)).sum / n.toDouble
  }

  // Define a function that returns the median of the first n prime numbers
  def medianPrimes(n: Int): Double = {
    val primes = firstPrimes(n)
    if (primes.size % 2 == 0) (primes(primes.size / 2 - 1) + primes(primes.size / 2)) / 2.0
    else primes(primes.size / 2)
  }

  // Define a function that returns the median of the first n even numbers
  def medianEvens(n: Int): Double = {
    val evens = (1 to n).filter(isEven)
    if (evens.size % 2 == 0) (evens(evens.size / 2 - 1) + evens(evens.size / 2)) / 2.0
    else evens(evens.size / 2)
  }

  // Define a function that returns the median of the first n odd numbers
  def medianOdds(n: Int): Double = {
    val odds = (1 to n).filter(!isEven(_))
    if (odds.size % 2 == 0) (odds(odds.size / 2 - 1) + odds(odds.size / 2)) / 2.0
    else odds(odds.size / 2)
  }

  // Define a function that returns the mode of the first n prime numbers
  def modePrimes(n: Int): Int = {
    val primes = firstPrimes(n)
    val counts = primes.groupBy(x => x).mapValues(_.size)
    val maxCount = counts.values.max
    counts.filter(_._2 == maxCount).keys.head
  }

  // Define a function that returns the mode of the first n even numbers
  def modeEvens(n: Int): Int = {
    val evens = (1 to n).filter(isEven)
    val counts = evens.groupBy(x => x).mapValues(_.size)
    val maxCount = counts.values.max
    counts.filter(_._2 == maxCount).keys.head
  }

  // Define a function that returns the mode of the first n odd numbers
  def modeOdds(n: Int): Int = {
    val odds = (1 to n).filter(!isEven(_))
    val counts = odds.groupBy(x => x).mapValues(_.size)
    val maxCount = counts.values.max
    counts.filter(_._2 == maxCount).keys.head
  }

  // Define a function that returns the range of the first n prime numbers
  def rangePrimes(n: Int): Int = {
    val primes = firstPrimes(n)
    primes.max - primes.min
  }

  // Define a function that returns the range of the first n even numbers
  def rangeEvens(n: Int): Int = {
    val evens = (1 to n).filter(isEven)
    evens.max - evens.min
  }

  // Define a function that returns the range of the first n odd numbers
  def rangeOdds(n: Int): Int = {
    val odds = (1 to n).filter(!isEven(_))
    odds.max - odds.min
  }

  // Define a function that returns the standard deviation of the first n prime numbers
  def stdDevPrimes(n: Int): Double = {
    val primes = firstPrimes(n)
    val mean = primes.sum / n.toDouble
    math.sqrt(primes.map(x => math.pow(x - mean, 2)).sum / (n-1))
  }

  // Define a function that returns the standard deviation of the first n even numbers
  def stdDevEvens(n: Int): Double = {
    val evens = (1 to n).filter(isEven)
    val mean = evens.sum / n.toDouble
    math.sqrt(e