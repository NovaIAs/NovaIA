```scala
object ComplexScalaCode {

  // Define a higher-order function that takes a function as a parameter and returns a new function
  def withFilter(filter: Int => Boolean): List[Int] => List[Int] = {
    (list: List[Int]) => list.filter(filter)
  }

  // Define a function that checks if a number is even
  def isEven: Int => Boolean = {
    (x: Int) => x % 2 == 0
  }

  // Define a function that checks if a number is odd
  def isOdd: Int => Boolean = {
    (x: Int) => x % 2 != 0
  }

  // Define a function that checks if a number is prime
  def isPrime: Int => Boolean = {
    (x: Int) =>
      (2 until x).forall(i => x % i != 0)
  }

  // Using the withFilter function, create a function that filters out even numbers from a list
  val filterEven: List[Int] => List[Int] = withFilter(isEven)

  // Using the withFilter function, create a function that filters out odd numbers from a list
  val filterOdd: List[Int] => List[Int] = withFilter(isOdd)

  // Using the withFilter function, create a function that filters out prime numbers from a list
  val filterPrime: List[Int] => List[Int] = withFilter(isPrime)

  // Create a list of integers from 1 to 100
  val numbers = (1 to 100).toList

  // Use the filterEven function to filter out even numbers from the list
  val evenNumbers = filterEven(numbers)

  // Use the filterOdd function to filter out odd numbers from the list
  val oddNumbers = filterOdd(numbers)

  // Use the filterPrime function to filter out prime numbers from the list
  val primeNumbers = filterPrime(numbers)

  // Print the results to the console
  println("Even numbers:")
  println(evenNumbers)

  println("Odd numbers:")
  println(oddNumbers)

  println("Prime numbers:")
  println(primeNumbers)
}
```

Explanation:

1. We define a higher-order function `withFilter` that takes a function as a parameter and returns a new function. This function allows us to easily create new filtering functions without having to write a lot of boilerplate code.

2. We define three helper functions: `isEven`, `isOdd`, and `isPrime`. These functions check if a number is even, odd, or prime, respectively.

3. We use the `withFilter` function to create three filtering functions: `filterEven`, `filterOdd`, and `filterPrime`. These functions filter out even, odd, and prime numbers from a list, respectively.

4. We create a list of integers from 1 to 100.

5. We use the `filterEven`, `filterOdd`, and `filterPrime` functions to filter out even, odd, and prime numbers from the list, respectively.

6. Finally, we print the results to the console.

This code demonstrates the power of higher-order functions in Scala and how they can be used to create complex filtering functions without having to write a lot of boilerplate code.