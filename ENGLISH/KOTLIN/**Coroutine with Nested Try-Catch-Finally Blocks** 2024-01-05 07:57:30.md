```kotlin
// Kotlin code to demonstrate the working of
// Coroutine with try catch and finally
// with Nested Try Catch Finally

import kotlinx.coroutines.*

fun main(args: Array<String>) {
    // Creating a scope to run the coroutine
    val scope = CoroutineScope(Dispatchers.IO)

    // Launching a new coroutine in the scope
    scope.launch {

        // Creating a try block
        try {

            // Creating a nested try-catch block
            try {

                // Creating a nested try-finally block
                try {

                    // Suspending the coroutine
                    delay(500)

                    // Throwing an exception
                    throw Exception("Exception in innermost try block")

                } // Nested try block ends here

                // Catching the exception in the nested try-finally block
                catch (e: Exception) {

                    // Printing the error message
                    println("Exception caught in nested try-finally block: ${e.message}")
                } // Nested catch block ends here

                // Finally block of the nested try-finally block
                finally {

                    // Printing a message
                    println("Finally block of nested try-finally block executed")
                } // Nested finally block ends here

            } // Nested try-catch block ends here

            // Catching the exception in the outer try block
            catch (e: Exception) {

                // Printing the error message
                println("Exception caught in outer try block: ${e.message}")
            } // Outer catch block ends here

            // Finally block of the outer try block
            finally {

                // Printing a message
                println("Finally block of outer try block executed")
            } // Outer finally block ends here

        } // Outer try block ends here

        // Catching the exception in the coroutine scope
        catch (e: Exception) {

            // Printing the error message
            println("Exception caught in coroutine scope: ${e.message}")
        } // Coroutine scope catch block ends here

        // Finally block of the coroutine scope
        finally {

            // Printing a message
            println("Finally block of coroutine scope executed")
        } // Coroutine scope finally block ends here
    } // Coroutine ends here
}
```

**Explanation:**

1. In Kotlin, a `coroutine` is a lightweight thread that can be paused and resumed at any point.
2. The `try-catch-finally` block is used to handle exceptions in Kotlin.
3. In this code, we have a `coroutine` that runs a `try-catch-finally` block.
4. Inside the `try` block, we have a nested `try-catch-finally` block.
5. In the innermost `try` block, we suspend the coroutine using the `delay()` function.
6. After the `delay()`, we throw an exception using `throw Exception("Exception in innermost try block")`.
7. The exception is caught in the nested `try-catch` block and a message is printed.
8. The `finally` block of the nested `try-catch-finally` block is then executed.
9. The exception is then caught in the outer `try-catch` block and a message is printed.
10. The `finally` block of the outer `try-catch-finally` block is then executed.
11. The exception is then caught in the `coroutine` scope `try-catch` block and a message is printed.
12. The `finally` block of the `coroutine` scope is then executed.