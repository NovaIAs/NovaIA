```scala
// Define an immutable list of integers
val numbers = List(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

// Define a function to calculate the factorial of a number
def factorial(n: Int): BigInt = {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}

// Define a function to calculate the sum of the factorials of a list of numbers
def sumFactorials(numbers: List[Int]): BigInt = {
  numbers.map(factorial).sum
}

// Define a function to calculate the prime factors of a number
def primeFactors(n: Int): List[Int] = {
  def isPrime(n: Int): Boolean = {
    (2 until n).forall(n % _ != 0)
  }

  def primeFactorsRec(n: Int, factors: List[Int]): List[Int] = {
    if (n == 1) {
      factors
    } else {
      val primeFactor = (2 to n).find(isPrime).get
      primeFactorsRec(n / primeFactor, primeFactor :: factors)
    }
  }

  primeFactorsRec(n, Nil)
}

// Define a function to calculate the greatest common divisor of two numbers
def gcd(a: Int, b: Int): Int = {
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}

// Define a function to calculate the least common multiple of two numbers
def lcm(a: Int, b: Int): Int = {
  a * b / gcd(a, b)
}

// Define a function to find the longest common subsequence of two strings
def longestCommonSubsequence(s1: String, s2: String): String = {
  def lcsRec(i: Int, j: Int): String = {
    if (i == s1.length || j == s2.length) {
      ""
    } else if (s1(i) == s2(j)) {
      s1(i) + lcsRec(i + 1, j + 1)
    } else {
      val lcs1 = lcsRec(i + 1, j)
      val lcs2 = lcsRec(i, j + 1)
      if (lcs1.length > lcs2.length) {
        lcs1
      } else {
        lcs2
      }
    }
  }

  lcsRec(0, 0)
}
```

**Explanation:**

1. Define a list of integers.
2. Define a function to calculate the factorial of a number.
3. Define a function to calculate the sum of the factorials of a list of numbers.
4. Define a function to calculate the prime factors of a number.
5. Define a function to calculate the greatest common divisor of two numbers.
6. Define a function to calculate the least common multiple of two numbers.
7. Define a function to find the longest common subsequence of two strings.