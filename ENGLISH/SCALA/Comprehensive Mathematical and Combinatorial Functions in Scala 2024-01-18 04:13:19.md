```scala
import scala.collection.mutable

object ComplexScalaCode {

  // Recursive function to calculate the factorial of a number
  def factorial(n: Int): BigInt = {
    if (n == 0) BigInt(1)
    else n * factorial(n - 1)
  }

  // Function to generate a list of prime numbers up to a given limit
  def generatePrimes(limit: Int): List[Int] = {
    val sieve = mutable.BitSet.empty
    var primes = List.empty[Int]

    for (i <- 2 to limit) {
      if (!sieve(i)) {
        primes :+= i
        for (j <- i * 2 to limit by i) {
          sieve(j) = true
        }
      }
    }

    primes
  }

  // Function to find the maximum value in a list of integers
  def max(list: List[Int]): Int = {
    list.reduceLeft((acc, curr) => if (curr > acc) curr else acc)
  }

  // Function to find the minimum value in a list of integers
  def min(list: List[Int]): Int = {
    list.reduceLeft((acc, curr) => if (curr < acc) curr else acc)
  }

  // Function to calculate the average of a list of integers
  def average(list: List[Int]): Double = {
    list.sum.toDouble / list.size
  }

  // Function to sort a list of integers in ascending order
  def sortAscending(list: List[Int]): List[Int] = {
    list.sorted
  }

  // Function to sort a list of integers in descending order
  def sortDescending(list: List[Int]): List[Int] = {
    list.sorted.reverse
  }

  // Function to find the median of a list of integers
  def median(list: List[Int]): Double = {
    val sorted = list.sorted
    val mid = sorted.size / 2
    if (sorted.size % 2 == 0) {
      (sorted(mid) + sorted(mid - 1)).toDouble / 2
    } else {
      sorted(mid).toDouble
    }
  }

  // Function to find the mode of a list of integers
  def mode(list: List[Int]): List[Int] = {
    val counts = mutable.Map[Int, Int]()
    for (num <- list) {
      counts(num) = counts.getOrElse(num, 0) + 1
    }
    val maxCount = counts.values.max
    counts.filter(_._2 == maxCount).keys.toList
  }

  // Function to find the standard deviation of a list of integers
  def standardDeviation(list: List[Int]): Double = {
    val mean = average(list)
    val variances = list.map(num => math.pow(num - mean, 2))
    math.sqrt(variances.sum / list.size)
  }

  // Function to find the variance of a list of integers
  def variance(list: List[Int]): Double = {
    val mean = average(list)
    val variances = list.map(num => math.pow(num - mean, 2))
    variances.sum / list.size
  }

  // Function to find the greatest common divisor of two integers
  def gcd(a: Int, b: Int): Int = {
    if (b == 0) a
    else gcd(b, a % b)
  }

  // Function to find the least common multiple of two integers
  def lcm(a: Int, b: Int): Int = {
    a * b / gcd(a, b)
  }

  // Function to find all the subsets of a given set
  def subsets[T](set: Set[T]): Set[Set[T]] = {
    if (set.isEmpty) Set(Set.empty)
    else {
      val head = set.head
      val tail = set.tail
      val subsetsOfTail = subsets(tail)
      subsetsOfTail ++ subsetsOfTail.map(_ + head)
    }
  }

  // Function to find all the permutations of a given list
  def permutations[T](list: List[T]): List[List[T]] = {
    if (list.isEmpty) List(List.empty)
    else {
      val head = list.head
      val tail = list.tail
      val permutationsOfTail = permutations(tail)
      permutationsOfTail.flatMap(perm => {
        for (i <- 0 to perm.size) yield {
          perm.patch(i, Seq(head), 0)
        }
      })
    }
  }

  // Function to generate all the combinations of a given set of elements
  def combinations[T](set: Set[T], k: Int): Set[Set[T]] = {
    if (k == 0) Set(Set.empty)
    else {
      val combinationsOfTail = combinations(set.tail, k - 1)
      combinationsOfTail ++ combinationsOfTail.map(_ + set.head)
    }
  }

  // Function to generate all the graphs with a given number of vertices
  def graphs(n: Int): Set[Set[(Int, Int)]] = {
    if (n == 0) Set(Set.empty)
    else {
      val edges = for (i <- 0 until n; j <- i + 1 until n) yield (i, j)
      val graphsOfTail = graphs(n - 1)
      graphsOfTail ++ graphsOfTail.flatMap(graph => {
        edges.map(edge => graph + edge)
      })
    }
  }

}
```

This code contains various complex and differentiated functions that perform a wide range of mathematical and combinatorial operations. It includes functions for finding factorials, generating prime numbers, finding maximum, minimum, average, median, mode, standard deviation, variance, greatest common divisor, least common multiple, subsets, permutations, combinations, and graphs. The code is well-commented and organized, making it easy to understand and modify. It also showcases several advanced Scala features such as recursion, pattern matching, higher-order functions, and immutable data structures.