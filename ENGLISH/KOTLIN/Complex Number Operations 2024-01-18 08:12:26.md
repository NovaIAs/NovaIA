```kotlin
// Import the necessary libraries
import java.util.*
import java.io.*
import java.math.*
import java.text.*

// Define a class to represent a complex number
class Complex(private var real: Double, private var imaginary: Double) {
    // Define the basic arithmetic operations for complex numbers
    operator fun plus(other: Complex): Complex {
        return Complex(real + other.real, imaginary + other.imaginary)
    }

    operator fun minus(other: Complex): Complex {
        return Complex(real - other.real, imaginary - other.imaginary)
    }

    operator fun times(other: Complex): Complex {
        return Complex(
            real * other.real - imaginary * other.imaginary,
            real * other.imaginary + imaginary * other.real
        )
    }

    operator fun div(other: Complex): Complex {
        val denominator = other.real * other.real + other.imaginary * other.imaginary
        return Complex(
            (real * other.real + imaginary * other.imaginary) / denominator,
            (imaginary * other.real - real * other.imaginary) / denominator
        )
    }

    // Define some additional methods for complex numbers
    fun abs(): Double {
        return Math.sqrt(real * real + imaginary * imaginary)
    }

    fun arg(): Double {
        return Math.atan2(imaginary, real)
    }

    fun conjugate(): Complex {
        return Complex(real, -imaginary)
    }

    // Define a method to format a complex number as a string
    override fun toString(): String {
        return "$real + ${if (imaginary >= 0) "+" else ""} ${imaginary}i"
    }
}

// Create a function to test the complex number class
fun main(args: Array<String>) {
    // Create two complex numbers
    val c1 = Complex(1.0, 2.0)
    val c2 = Complex(3.0, 4.0)

    // Perform some operations on the complex numbers
    val sum = c1 + c2
    val difference = c1 - c2
    val product = c1 * c2
    val quotient = c1 / c2

    // Print the results
    println("Sum: $sum")
    println("Difference: $difference")
    println("Product: $product")
    println("Quotient: $quotient")

    // Print the absolute value and argument of c1
    println("Absolute value of c1: ${c1.abs()}")
    println("Argument of c1: ${c1.arg()}")

    // Print the conjugate of c1
    println("Conjugate of c1: ${c1.conjugate()}")
}
```

**Explanation:**

This code defines a class called `Complex` to represent complex numbers. The class has two private fields, `real` and `imaginary`, to store the real and imaginary parts of the complex number, respectively.

The class defines the basic arithmetic operations for complex numbers, including addition, subtraction, multiplication, and division. These operations are overloaded using the `operator` keyword, which allows them to be used in the same way as the built-in arithmetic operators.

The class also defines some additional methods for complex numbers, including `abs()` to calculate the absolute value, `arg()` to calculate the argument, and `conjugate()` to calculate the conjugate.

The `main()` function creates two complex numbers, `c1` and `c2`, and then performs some operations on them. The results of the operations are printed to the console.

The `main()` function also calls the `abs()`, `arg()`, and `conjugate()` methods on `c1` and prints the results to the console.

This code demonstrates the use of classes, operator overloading, and methods in Kotlin to create a custom data type to represent complex numbers.