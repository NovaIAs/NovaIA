```kotlin
// This is a program that calculates the factorial of a given number.

// A function to calculate the factorial of a number.
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// A function to print the factorial of a number.
fun printFactorial(n: Int) {
    val result = factorial(n)
    println("The factorial of $n is $result.")
}

// A function to get the input from the user.
fun getInput(): Int {
    val scanner = Scanner(System.`in`)
    println("Enter a non-negative integer:")
    val input = scanner.nextInt()
    return input
}

// The main function.
fun main(args: Array<String>) {
    val input = getInput()
    printFactorial(input)
}
```

This program first defines a function called `factorial` that takes an integer as input and returns the factorial of that integer. The factorial of a number is the product of all the positive integers less than or equal to that number. For example, the factorial of 5 is `5 x 4 x 3 x 2 x 1 = 120`.

The `factorial` function uses recursion to calculate the factorial of a number. Recursion is a technique where a function calls itself. In this case, the `factorial` function calls itself with the input minus one. This process continues until the input is equal to 0, at which point the function returns 1.

The program then defines a function called `printFactorial` that takes an integer as input and prints the factorial of that integer.

The program then defines a function called `getInput` that gets the input from the user.

Finally, the program calls the `main` function, which calls the `getInput` function to get the input from the user and then calls the `printFactorial` function to print the factorial of the input.