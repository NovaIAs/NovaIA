```kotlin
// Kotlin code to demonstrate the implementation of
// Complex Types and Extensions

// Defining complex types
class Complex(real: Double, imaginary: Double) {

    // Properties of the complex number
    var real: Double = real
    var imaginary: Double = imaginary

    // Overriding the toString() method
    override fun toString(): String {
        return if (imaginary < 0) {
            "$real - ${-imaginary}i"
        } else {
            "$real + $imaginaryi"
        }
    }
}

// Extending the complex type to provide additional functionality
fun Complex.conjugate(): Complex {
    return Complex(real, -imaginary)
}

fun main(args: Array<String>) {
    // Creating a complex number
    val c1 = Complex(3.0, 4.0)
    val c2 = Complex(5.0, -2.0)

    // Displaying the complex numbers
    println("c1: $c1")
    println("c2: $c2")

    // Using the conjugate extension function
    val c1Conjugate = c1.conjugate()
    val c2Conjugate = c2.conjugate()

    // Displaying the conjugates
    println("c1 Conjugate: $c1Conjugate")
    println("c2 Conjugate: $c2Conjugate")
}
```

Explanation:

1. `Complex` Class:
   - We define a `Complex` class to represent complex numbers.
   - It has two properties: `real` and `imaginary` to store the real and imaginary parts of the complex number.
   - The class overrides the `toString()` method to provide a custom string representation of the complex number.

2. `conjugate()` Extension Function:
   - We extend the `Complex` class with a `conjugate()` extension function.
   - This function returns the complex conjugate of the current complex number, where the imaginary part is negated.

3. `main()` Function:
   - We create two `Complex` number instances, `c1` and `c2`, with different real and imaginary parts.
   - We display the original complex numbers using `println()`.
   - We use the `conjugate()` extension function to compute the conjugates of `c1` and `c2` and display them.

This code demonstrates the use of complex types and extension functions in Kotlin. It represents complex numbers as objects with real and imaginary parts, and provides a convenient way to compute the complex conjugate using an extension function.