```kotlin
// Kotlin code to demonstrate the working of
// Lambda functions with Higher Order Functions

// Creating a Higher Order Function
fun higherOrderFunction(function: (Int) -> Int): Int {
    // Returning the result of function
    // passed as an argument
    return function(5)
}

// Creating a Lambda function
val lambdaFunction: (Int) -> Int = { num -> num * num }

// Using the Lambda function inside the Higher Order Function
val result = higherOrderFunction(lambdaFunction)

// Displaying the result
println(result) // Output: 25
```

**Explanation:**

1. **Creating a Higher Order Function:**
   - `higherOrderFunction` is a higher-order function that takes a lambda function as an argument and returns an integer.

2. **Creating a Lambda Function:**
   - `lambdaFunction` is a lambda function that takes an integer `num` as an argument and returns `num * num`.

3. **Using the Lambda Function inside the Higher Order Function:**
   - We pass the `lambdaFunction` as an argument to the `higherOrderFunction`. The `lambdaFunction` is executed inside the `higherOrderFunction`, and the result is returned.

4. **Displaying the Result:**
   - The result of the `higherOrderFunction` is printed to the console, which is `25` in this case.

This code demonstrates how lambda functions can be used inside higher-order functions, providing a concise and expressive way to pass functions as arguments.