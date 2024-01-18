```scala
object ComplexScalaCode {

  // Define a higher-order function that takes a function as an argument.
  def applyFunction(f: Int => Int, x: Int): Int = {
    // Call the function f with the argument x.
    f(x)
  }

  // Define a function that takes an Int and returns its square.
  def square(x: Int): Int = {
    x * x
  }

  // Define a function that takes an Int and returns its cube.
  def cube(x: Int): Int = {
    x * x * x
  }

  // Call the applyFunction function with the square function and the argument 5.
  val result1 = applyFunction(square, 5)

  // Call the applyFunction function with the cube function and the argument 5.
  val result2 = applyFunction(cube, 5)

  // Print the results.
  println(result1) // Prints 25
  println(result2) // Prints 125
}
```

Explanation:

1. The `applyFunction` function takes a function as an argument and applies it to a given input. This allows us to pass different functions to the `applyFunction` function and perform different operations on the input.
2. The `square` function takes an Int and returns its square.
3. The `cube` function takes an Int and returns its cube.
4. In the main function, we call the `applyFunction` function with the `square` function and the argument `5`. This calls the `square` function with the argument `5` and returns the result, which is `25`.
5. We then call the `applyFunction` function with the `cube` function and the argument `5`. This calls the `cube` function with the argument `5` and returns the result, which is `125`.
6. Finally, we print the results to the console.