```scala
// Define a higher-order function that takes a function and a list as arguments
def map[A, B](f: A => B, list: List[A]): List[B] = list match {
  case Nil => Nil
  case head :: tail => f(head) :: map(f, tail)
}

// Define a function that takes an integer and returns its square
def square(x: Int): Int = x * x

// Define a function that takes a string and returns its length
def length(s: String): Int = s.length

// Define a list of integers
val numbers = List(1, 2, 3, 4, 5)

// Define a list of strings
val strings = List("hello", "world", "!")

// Apply the map function to the list of integers using the square function
val squaredNumbers = map(square, numbers)

// Apply the map function to the list of strings using the length function
val stringLengths = map(length, strings)

// Print the squared numbers
println(squaredNumbers)

// Print the string lengths
println(stringLengths)
```

Explanation:

1. The map function is defined as a higher-order function that takes a function (f) and a list (list) as arguments. It applies the function to each element of the list and returns a new list containing the results.

2. The square function is defined to take an integer and return its square.

3. The length function is defined to take a string and return its length.

4. The numbers variable is defined as a list of integers.

5. The strings variable is defined as a list of strings.

6. The squaredNumbers variable is assigned the result of applying the map function to the list of integers using the square function.

7. The stringLengths variable is assigned the result of applying the map function to the list of strings using the length function.

8. The squaredNumbers and stringLengths variables are printed to the console.

This code demonstrates the use of higher-order functions, functional programming, and list processing in Scala. It shows how to apply a function to each element of a list and produce a new list containing the results. It also demonstrates the use of lambda expressions (anonymous functions) and the concise syntax of Scala.