```kotlin
// This is a Kotlin program to demonstrate the use of lambda expressions.

// A lambda expression is a block of code that can be passed as an argument to a function.
// It is also known as an anonymous function or a closure.

// Lambda expressions are enclosed in curly braces {} and can have one or more parameters.
// The body of the lambda expression is the code that is executed when the lambda expression is called.

// Here is an example of a lambda expression that takes two arguments and returns their sum:
val sum = { x: Int, y: Int -> x + y }

// Lambda expressions can be passed to functions as arguments.
// Here is an example of a function that takes a lambda expression as an argument:
fun apply(f: (Int, Int) -> Int, x: Int, y: Int) {
    val result = f(x, y)
    println("The result is $result")
}

// Here is an example of how to use the apply function:
apply(sum, 1, 2)

// Lambda expressions can also be used to create anonymous classes.
// Here is an example of an anonymous class that implements the Runnable interface:
val runnable = Runnable {
    println("Hello world!")
}

// Here is an example of how to use the runnable:
runnable.run()

// Lambda expressions are a powerful tool that can be used to improve the readability and maintainability of your Kotlin code.
```

**Explanation:**

This Kotlin program demonstrates the use of lambda expressions.

A lambda expression is a block of code that can be passed as an argument to a function. It is also known as an anonymous function or a closure.

Lambda expressions are enclosed in curly braces {} and can have one or more parameters. The body of the lambda expression is the code that is executed when the lambda expression is called.

In this program, we define a lambda expression called `sum` that takes two arguments and returns their sum. We then pass the `sum` lambda expression to the `apply` function as an argument. The `apply` function takes a lambda expression as an argument and executes the lambda expression with the specified arguments.

We also define an anonymous class that implements the `Runnable` interface. The anonymous class contains a lambda expression that prints "Hello world!" to the console. We then create an instance of the anonymous class and call the `run` method to execute the lambda expression.

Lambda expressions are a powerful tool that can be used to improve the readability and maintainability of your Kotlin code.