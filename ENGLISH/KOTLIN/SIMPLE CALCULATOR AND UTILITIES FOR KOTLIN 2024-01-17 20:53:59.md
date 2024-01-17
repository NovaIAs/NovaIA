```kotlin
// A simple Kotlin program to demonstrate higher-order functions
// and lambda expressions

// Function type alias for a function that takes two integers
// and returns an integer
typealias IntBinaryOperator = (Int, Int) -> Int

// Function type alias for a function that takes an integer and returns
// a boolean value
typealias IntPredicate = (Int) -> Boolean

// Function that takes two integer numbers and returns their sum
fun add(a: Int, b: Int): Int {
    return a + b
}

// Function that takes two integer numbers and returns their difference
fun subtract(a: Int, b: Int): Int {
    return a - b
}

// Function that takes two integer numbers and returns their product
fun multiply(a: Int, b: Int): Int {
    return a * b
}

// Function that takes two integer numbers and returns their quotient
fun divide(a: Int, b: Int): Int {
    return a / b
}

// Function that takes an integer number and returns its square
fun square(a: Int): Int {
    return a * a
}

// Function that takes an integer number and returns its cube
fun cube(a: Int): Int {
    return a * a * a
}

// Function that takes an integer number and returns its factorial
fun factorial(a: Int): Int {
    if (a == 0) {
        return 1
    } else {
        return a * factorial(a - 1)
    }
}

// Function that takes an integer number and returns true if it is
// even, and false otherwise
fun isEven(a: Int): Boolean {
    return a % 2 == 0
}

// Function that takes an integer number and returns true if it is
// odd, and false otherwise
fun isOdd(a: Int): Boolean {
    return a % 2 == 1
}

// Function that takes an integer number and returns true if it is
// prime, and false otherwise
fun isPrime(a: Int): Boolean {
    if (a <= 1) {
        return false
    }
    for (i in 2..a / 2) {
        if (a % i == 0) {
            return false
        }
    }
    return true
}

// Function that takes an integer number and returns the sum of its digits
fun sumOfDigits(a: Int): Int {
    var sum = 0
    var n = a
    while (n > 0) {
        sum += n % 10
        n /= 10
    }
    return sum
}

// Function that takes an integer number and returns the product of its digits
fun productOfDigits(a: Int): Int {
    var product = 1
    var n = a
    while (n > 0) {
        product *= n % 10
        n /= 10
    }
    return product
}

// Function that takes an integer number and returns the number of digits
fun numberOfDigits(a: Int): Int {
    var count = 0
    var n = a
    while (n > 0) {
        count++
        n /= 10
    }
    return count
}

// Function that takes an integer number and returns its reverse
fun reverse(a: Int): Int {
    var reverse = 0
    var n = a
    while (n > 0) {
        reverse = reverse * 10 + n % 10
        n /= 10
    }
    return reverse
}

// Function that takes an integer number and returns true if it is
// a palindrome, and false otherwise
fun isPalindrome(a: Int): Boolean {
    return a == reverse(a)
}

// Function that takes an integer number and returns its binary
// representation as a string
fun toBinaryString(a: Int): String {
    var binaryString = ""
    var n = a
    while (n > 0) {
        binaryString = (n % 2).toString() + binaryString
        n /= 2
    }
    return binaryString
}

// Function that takes an integer number and returns its hexadecimal
// representation as a string
fun toHexString(a: Int): String {
    var hexadecimalString = ""
    var n = a
    while (n > 0) {
        val