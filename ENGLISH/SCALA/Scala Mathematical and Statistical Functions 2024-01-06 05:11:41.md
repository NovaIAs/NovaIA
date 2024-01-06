```scala
object ComplexScalaCode {

  // Define a class to represent a complex number
  case class Complex(real: Double, imaginary: Double) {
    def +(other: Complex): Complex = Complex(real + other.real, imaginary + other.imaginary)
    def -(other: Complex): Complex = Complex(real - other.real, imaginary - other.imaginary)
    def *(other: Complex): Complex = Complex(real * other.real - imaginary * other.imaginary,
                                            real * other.imaginary + imaginary * other.real)
    def /(other: Complex): Complex = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary
      Complex((real * other.real + imaginary * other.imaginary) / denominator,
             (imaginary * other.real - real * other.imaginary) / denominator)
    }
    override def toString: String = s"($real, $imaginary)"
  }

  // Define a function to calculate the factorial of a number
  def factorial(n: Int): Int = {
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  // Define a function to calculate the Fibonacci sequence up to a certain number
  def fibonacci(n: Int): Int = {
    if (n <= 1) n
    else fibonacci(n - 1) + fibonacci(n - 2)
  }

  // Define a function to check if a number is prime
  def isPrime(n: Int): Boolean = {
    if (n <= 1) false
    else if (n == 2) true
    else !(2 to Math.sqrt(n).toInt).exists(n % _ == 0)
  }

  // Define a function to find the greatest common divisor of two numbers
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  // Define a function to find the least common multiple of two numbers
  def lcm(a: Int, b: Int): Int = {
    a * b / gcd(a, b)
  }

  // Define a function to generate a random list of integers
  def randomList(n: Int): List[Int] = {
    (1 to n).map(_ => scala.util.Random.nextInt(100))
  }

  // Define a function to sort a list of integers in ascending order
  def sortAscending(list: List[Int]): List[Int] = {
    list.sorted
  }

  // Define a function to sort a list of integers in descending order
  def sortDescending(list: List[Int]): List[Int] = {
    list.sorted(Ordering[Int].reverse)
  }

  // Define a function to find the maximum value in a list of integers
  def max(list: List[Int]): Int = {
    list.max
  }

  // Define a function to find the minimum value in a list of integers
  def min(list: List[Int]): Int = {
    list.min
  }

  // Define a function to calculate the average of a list of integers
  def average(list: List[Int]): Double = {
    list.sum.toDouble / list.length
  }

  // Define a function to find the median of a list of integers
  def median(list: List[Int]): Double = {
    val sortedList = list.sorted
    val mid = sortedList.length / 2
    if (sortedList.length % 2 == 0) {
      (sortedList(mid - 1) + sortedList(mid)) / 2.0
    } else {
      sortedList(mid)
    }
  }

  // Define a function to find the mode of a list of integers
  def mode(list: List[Int]): List[Int] = {
    val frequencies = list.groupBy(identity).mapValues(_.length)
    val maxFrequency = frequencies.values.max
    frequencies.filter(_._2 == maxFrequency).keys.toList
  }

  // Define a function to find the range of a list of integers
  def range(list: List[Int]): Int = {
    max(list) - min(list)
  }

  // Define a function to find the variance of a list of integers
  def variance(list: List[Int]): Double = {
    val mean = average(list)
    list.map(x => Math.pow(x - mean, 2)).sum / list.length
  }

  // Define a function to find the standard deviation of a list of integers
  def standardDeviation(list: List[Int]): Double = {
    Math.sqrt(variance(list))
  }

  // Define a function to find the correlation coefficient between two lists of integers
  def correlationCoefficient(list1: List[Int], list2: List[Int]): Double = {
    val mean1 = average(list1)
    val mean2 = average(list2)
    val numerator = list1.zip(list2).map(x => (x._1 - mean1) * (x._2 - mean2)).sum
    val denominator = Math.sqrt(variance(list1) * variance(list2