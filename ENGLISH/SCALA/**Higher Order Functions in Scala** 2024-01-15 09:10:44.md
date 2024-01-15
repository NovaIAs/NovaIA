```scala
// Scala program to demonstrate the use of higher order functions

// Define a function to calculate the sum of a list of integers
def sum(numbers: List[Int]): Int = {
  // Use the foldLeft function to accumulate the sum of the list
  numbers.foldLeft(0)((sum, number) => sum + number)
}

// Define a function to calculate the average of a list of integers
def average(numbers: List[Int]): Double = {
  // Use the sum function to calculate the sum of the list
  val sum = sum(numbers)

  // Use the length function to calculate the number of elements in the list
  val count = numbers.length

  // Calculate the average by dividing the sum by the count
  sum.toDouble / count
}

// Define a function to calculate the maximum value in a list of integers
def max(numbers: List[Int]): Int = {
  // Use the foldLeft function to find the maximum value in the list
  numbers.foldLeft(Int.MinValue)((max, number) => if (number > max) number else max)
}

// Define a function to calculate the minimum value in a list of integers
def min(numbers: List[Int]): Int = {
  // Use the foldLeft function to find the minimum value in the list
  numbers.foldLeft(Int.MaxValue)((min, number) => if (number < min) number else min)
}

// Define a function to filter a list of integers based on a predicate
def filter(numbers: List[Int], predicate: Int => Boolean): List[Int] = {
  // Use the filter function to filter the list based on the predicate
  numbers.filter(predicate)
}

// Define a function to map a list of integers to a new list of values
def map(numbers: List[Int], mapper: Int => Int): List[Int] = {
  // Use the map function to map the list to a new list of values
  numbers.map(mapper)
}

// Define a function to reduce a list of integers to a single value
def reduce(numbers: List[Int], reducer: (Int, Int) => Int): Int = {
  // Use the reduce function to reduce the list to a single value
  numbers.reduce(reducer)
}

// Define a function to sort a list of integers in ascending order
def sort(numbers: List[Int]): List[Int] = {
  // Use the sort function to sort the list in ascending order
  numbers.sortWith((a, b) => a < b)
}

// Define a function to reverse a list of integers
def reverse(numbers: List[Int]): List[Int] = {
  // Use the reverse function to reverse the list
  numbers.reverse
}

// Define a function to take a list of integers and return a tuple containing the sum, average, maximum, minimum, and sorted list
def summarize(numbers: List[Int]): (Int, Double, Int, Int, List[Int]) = {
  // Use the sum, average, max, min, and sort functions to calculate the summary statistics
  (sum(numbers), average(numbers), max(numbers), min(numbers), sort(numbers))
}

// Define a function to take a list of integers and return a list of even numbers
def evenNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for even numbers
  filter(numbers, _ % 2 == 0)
}

// Define a function to take a list of integers and return a list of odd numbers
def oddNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for odd numbers
  filter(numbers, _ % 2 != 0)
}

// Define a function to take a list of integers and return a list of prime numbers
def primeNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for prime numbers
  filter(numbers, isPrime)

  // Define a function to check if a number is prime
  def isPrime(number: Int): Boolean = {
    if (number <= 1) {
      false
    } else {
      !(2 to math.sqrt(number).toInt).exists(number % _ == 0)
    }
  }
}

// Define a function to take a list of integers and return a list of perfect numbers
def perfectNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for perfect numbers
  filter(numbers, isPerfect)

  // Define a function to check if a number is perfect
  def isPerfect(number: Int): Boolean = {
    if (number <= 1) {
      false
    } else {
      val divisors = (1 to math.sqrt(number).toInt).filter(number % _ == 0)
      number == divisors.sum
    }
  }
}

// Define a function to take a list of integers and return a list of amicable numbers
def amicableNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for amicable numbers
  filter(numbers, isAmicable)

  // Define a function to check if two numbers are amicable
  def isAmicable(number1: Int, number2: Int): Boolean = {
    val sumOfProperDivisors1 = (1 to math.sqrt(number1).toInt).filter(number1 % _ == 0).sum
    val sumOfProperDivisors2 = (1 to math.sqrt(number2).toInt).filter(number2 % _ == 0).sum
    number1 == sumOfProperDivisors2 && number2 == sumOfProperDivisors1
  }

  // Use the combinations function to generate pairs of numbers from the list
  numbers.combinations(2).filter(pair => isAmicable(pair(0), pair(1))).map(_(0))
}

// Define a function to take a list of integers and return a list of happy numbers
def happyNumbers(numbers: List[Int]): List[Int] = {
  // Use the filter function to filter the list for happy numbers
  filter(numbers, isHappy)

  // Define a function to check if a number is happy
  def isHappy(number: Int): Boolean = {
    var slow = number
    var fast = number * number

    while (slow != fast) {
      slow = sumOfDigitsSquared(slow)
      fast = sumOfDigitsSquared(sumOfDigitsSquared(fast))
    }

    slow == 1

    // Define a function to calculate the sum of the squares of the digits of a number
    def sumOfDigitsSquared(number: Int): Int = {
      number.toString.map(_.asDigit).map(math.pow(_, 2).toInt).sum
    }
  }
}
```

This code demonstrates the use of higher order functions in Scala. It defines a number of functions that can be used to manipulate and analyze lists of integers. These functions include:

* `sum`: Calculates the sum of a list of integers.
* `average`: Calculates the average of a list of integers.
* `max`: Finds the maximum value in a list of integers.
* `min`: Finds the minimum value in a list of integers.
* `filter`: Filters a list of integers based on a predicate.
* `map`: Maps a list of integers to a new list of values.
* `reduce`: Reduces a list of integers to a single value.
* `sort`: Sorts a list of integers in ascending order.
* `reverse`: Reverses a list of integers.
* `summarize`: Takes a list of integers and returns a tuple containing the sum, average, maximum, minimum, and sorted list.
* `evenNumbers`: Filters a list of integers for even numbers.
* `oddNumbers`: Filters a list of integers for odd numbers.
* `primeNumbers`: Filters a list of integers for prime numbers.
* `perfectNumbers`: Filters a list of integers for perfect numbers.
* `amicableNumbers`: Filters a list of integers for amicable numbers.
* `happyNumbers`: Filters a list of integers for happy numbers.

These functions can be used to perform a variety of tasks, such as calculating statistics, filtering data, and finding special numbers. They are also useful for writing more complex programs that can be used to solve a wide variety of problems.