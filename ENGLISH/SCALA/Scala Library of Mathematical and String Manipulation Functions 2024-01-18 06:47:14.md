```scala
// Define a function to calculate the factorial of a number
def factorial(n: Int): BigInt = {
  if (n == 0) {
    1
  } else {
    n * factorial(n - 1)
  }
}

// Define a function to calculate the Fibonacci sequence
def fibonacci(n: Int): Int = {
  if (n <= 1) {
    n
  } else {
    fibonacci(n - 1) + fibonacci(n - 2)
  }
}

// Define a function to check if a number is prime
def isPrime(n: Int): Boolean = {
  if (n <= 1) {
    false
  } else if (n == 2) {
    true
  } else {
    !(2 to math.sqrt(n).toInt).exists(i => n % i == 0)
  }
}

// Define a function to find the greatest common divisor of two numbers
def gcd(a: Int, b: Int): Int = {
  if (b == 0) {
    a
  } else {
    gcd(b, a % b)
  }
}

// Define a function to find the least common multiple of two numbers
def lcm(a: Int, b: Int): Int = {
  (a * b) / gcd(a, b)
}

// Define a function to check if a string is a palindrome
def isPalindrome(str: String): Boolean = {
  str == str.reverse
}

// Define a function to reverse a string
def reverseString(str: String): String = {
  str.reverse
}

// Define a function to find the longest common substring of two strings
def longestCommonSubstring(str1: String, str2: String): String = {
  val dp = Array.ofDim[Int](str1.length, str2.length)
  var longestLength = 0
  var longestSubstring = ""

  for (i <- 0 until str1.length) {
    for (j <- 0 until str2.length) {
      if (str1(i) == str2(j)) {
        if (i == 0 || j == 0) {
          dp(i)(j) = 1
        } else {
          dp(i)(j) = dp(i - 1)(j - 1) + 1
        }

        if (dp(i)(j) > longestLength) {
          longestLength = dp(i)(j)
          longestSubstring = str1.substring(i - longestLength + 1, i + 1)
        }
      }
    }
  }

  longestSubstring
}

// Test the functions
println(factorial(5))  // 120
println(fibonacci(10))  // 55
println(isPrime(13))  // true
println(gcd(12, 18))  // 6
println(lcm(9, 15))  // 45
println(isPalindrome("racecar"))  // true
println(reverseString("hello"))  // "olleh"
println(longestCommonSubstring("ABCDGH", "ACDGHR"))  // "CDGH"
```

This code contains a collection of common mathematical and string manipulation functions, including factorial calculation, Fibonacci sequence calculation, primality testing, greatest common divisor and least common multiple calculation, palindrome checking, string reversal, and longest common substring finding. It showcases a variety of Scala features, including recursion, pattern matching, and array manipulation.