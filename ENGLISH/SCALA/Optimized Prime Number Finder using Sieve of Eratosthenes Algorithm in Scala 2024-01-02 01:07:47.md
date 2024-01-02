Certainly! Here's a complex and differentiated code in Scala that implements an optimized algorithm for finding prime numbers using the Sieve of Eratosthenes.

```scala
object PrimeNumberFinder {
  def main(args: Array[String]): Unit = {
    val n = 1000000
    val primes = sieveOfEratosthenes(n)
    println("Prime numbers up to " + n + ":")
    primes.foreach(println)
  }

  def sieveOfEratosthenes(n: Int): List[Int] = {
    val isPrime = Array.fill(n + 1)(true)
    isPrime(0) = false
    isPrime(1) = false

    for (i <- 2 to Math.sqrt(n).toInt if isPrime(i)) {
      for (j <- i * i to n by i) {
        isPrime(j) = false
      }
    }

    (2 to n).filter(isPrime).toList
  }
}
```

This code defines an object `PrimeNumberFinder` with a `main` method that finds and prints all prime numbers up to a given number `n`. It utilizes the Sieve of Eratosthenes algorithm, which is an efficient method for finding prime numbers.

The `sieveOfEratosthenes` function takes an integer `n` as input and returns a list of prime numbers up to `n`. It uses an array `isPrime` to keep track of whether each number is prime or not. Initially, all numbers are considered prime except 0 and 1.

The outer loop iterates from 2 to the square root of `n`. For each prime number `i`, the inner loop marks all multiples of `i` as non-prime by setting the corresponding elements in the `isPrime` array to `false`.

Finally, the function uses the `filter` method to select only the prime numbers from the range 2 to `n` and converts them to a list.

In the `main` method, we set `n` to 1,000,000 and call the `sieveOfEratosthenes` function to find all prime numbers up to `n`. The resulting primes are then printed to the console.

Please note that this code is optimized and can handle large values of `n` efficiently.