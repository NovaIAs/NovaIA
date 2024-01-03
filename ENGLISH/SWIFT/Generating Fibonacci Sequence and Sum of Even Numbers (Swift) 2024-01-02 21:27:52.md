Sure! Here's a complex and unique code written in Swift that generates a Fibonacci sequence up to a given number, and then calculates the sum of all the even numbers in the sequence. Let me explain the code step by step:

```swift
func fibonacciSum(upTo limit: Int) -> Int {
    var sum = 0
    var a = 0
    var b = 1
    
    while a <= limit {
        if a % 2 == 0 {
            sum += a
        }
        
        let temp = a + b
        a = b
        b = temp
    }
    
    return sum
}

let limit = 1000
let sum = fibonacciSum(upTo: limit)
print("The sum of even Fibonacci numbers up to \(limit) is \(sum).")
```

In this code, we define a function called `fibonacciSum` that takes an upper limit as a parameter and returns the sum of all even Fibonacci numbers up to that limit.

Inside the function, we initialize three variables:
- `sum` holds the cumulative sum of the even Fibonacci numbers.
- `a` represents the current Fibonacci number.
- `b` represents the next Fibonacci number.

We enter a while loop that continues as long as the current Fibonacci number (`a`) is less than or equal to the given limit. Inside the loop, we check if the current Fibonacci number is even by using the modulo operator (`%`). If it is, we add it to the `sum` variable.

After that, we calculate the next Fibonacci number by storing the sum of the current Fibonacci number (`a`) and the previous Fibonacci number (`b`) in a temporary variable `temp`. We then update `a` to be the value of `b` and `b` to be the value of `temp`.

Once the loop finishes, we return the final sum of even Fibonacci numbers.

Finally, we set a limit value (in this case, 1000), call the `fibonacciSum` function with that limit, and print the result.

This code generates the Fibonacci sequence dynamically and calculates the sum of even numbers efficiently, making it a complex and differentiated solution.