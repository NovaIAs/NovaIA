```scala
// Fibonacci Sequence Calculator
// Calculates the Fibonacci sequence up to a specified number of terms.

// Define the main function.
def main(args: Array[String]): Unit = {

  // Define the function to calculate the Fibonacci sequence.
  def fibonacci(n: Int): BigInt = {
    if (n == 0 || n == 1) {
      n
    } else {
      fibonacci(n - 1) + fibonacci(n - 2)
    }
  }

  // Prompt the user to enter the number of terms.
  print("Enter the number of terms: ")
  val terms = scala.io.StdIn.readInt()

  // Calculate and print the Fibonacci sequence.
  for (i <- 0 until terms) {
    println(fibonacci(i))
  }
}
```

Explanation:

1. **Function Definition (`fibonacci` Function):**

   ```scala
   def fibonacci(n: Int): BigInt = {
     if (n == 0 || n == 1) {
       n
     } else {
       fibonacci(n - 1) + fibonacci(n - 2)
     }
   }
   ```

   - This function is defined to calculate the Fibonacci sequence. It takes an integer `n` as input and returns the `n`-th Fibonacci number as a `BigInt`.
   - The function uses recursion to compute the Fibonacci sequence. For base cases, it returns `n` itself when `n` is 0 or 1.
   - For other values of `n`, it recursively calls itself with `n - 1` and `n - 2` to calculate the Fibonacci numbers at those positions and then adds them to get the `n`-th Fibonacci number.

2. **Main Function (`main` Function):**

   ```scala
   def main(args: Array[String]): Unit = {

     // Prompt the user to enter the number of terms.
     print("Enter the number of terms: ")
     val terms = scala.io.StdIn.readInt()

     // Calculate and print the Fibonacci sequence.
     for (i <- 0 until terms) {
       println(fibonacci(i))
     }
   }
   ```

   - This is the entry point of the program, which is executed when you run the script.
   - It prompts the user to enter the number of terms they want to calculate in the Fibonacci sequence.
   - It then uses a `for` loop to iterate from 0 to the number of terms entered by the user. For each iteration, it calls the `fibonacci` function to calculate the `i`-th Fibonacci number and prints it.