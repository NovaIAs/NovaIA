**The Fibonacci Sequence Generator:**
```
import Cocoa

class Fibonacci {
  // Return the nth Fibonacci number.
  static func getValue(_ n: Int) -> Int {
    if n <= 1 {
      return n
    } else {
      return getValue(n - 1) + getValue(n - 2)
    }
  }

  // Generates the first `n` Fibonacci numbers as an array.
  static func generate(_ n: Int) -> [Int] {
    var sequence: [Int] = []
    for i in 0..<n {
      sequence.append(getValue(i))
    }
    return sequence
  }
}

// Sample usage:
let sequence = Fibonacci.generate(10)
print(sequence) // Output: [0, 1, 1, 2, 3, 5, 8, 13, 21, 34]

```
This Swift code defines a simple class called Fibonacci that generates Fibonacci numbers.

1. The Fibonacci class has a static function named getValue that calculates the nth Fibonacci number using a recursive approach.
2. If n is less than or equal to 1, it returns n, as those are the base cases for the Fibonacci sequence.
3. For other values of n, it recursively calls getValue with the arguments n - 1 and n - 2 and adds their results to compute the nth Fibonacci number.
4. The generate function is also a static function in the Fibonacci class. It generates an array of the first n Fibonacci numbers by calling getValue for each integer from 0 to n-1 and storing the results in the sequence array.
5. The sample usage section demonstrates how to use the class. It calls the generate function with the argument 10 to generate the first 10 Fibonacci numbers and prints them to the console.