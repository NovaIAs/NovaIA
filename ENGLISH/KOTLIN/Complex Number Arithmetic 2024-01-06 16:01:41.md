```kotlin
// This code creates a class called `Complex` that represents a complex number.
// Complex numbers have two parts: a real part and an imaginary part.
// The real part is the part that we're familiar with from everyday life,
// while the imaginary part is the part that involves the imaginary unit `i`.
// The imaginary unit is defined as the square root of -1.

class Complex(val real: Double, val imaginary: Double) {

    // This function adds two complex numbers together.
    // It does this by adding the real parts together and the imaginary parts together.
    operator fun plus(other: Complex): Complex {
        return Complex(real + other.real, imaginary + other.imaginary)
    }

    // This function subtracts one complex number from another.
    // It does this by subtracting the real parts and the imaginary parts.
    operator fun minus(other: Complex): Complex {
        return Complex(real - other.real, imaginary - other.imaginary)
    }

    // This function multiplies two complex numbers together.
    // It does this by using the distributive property to expand the product.
    operator fun times(other: Complex): Complex {
        return Complex(
            real * other.real - imaginary * other.imaginary,
            real * other.imaginary + imaginary * other.real
        )
    }

    // This function divides one complex number by another.
    // It does this by multiplying the numerator by the complex conjugate of the denominator.
    // The complex conjugate of a complex number is the same number with the imaginary part negated.
    operator fun div(other: Complex): Complex {
        val denominator = other.real * other.real + other.imaginary * other.imaginary
        return Complex(
            (real * other.real + imaginary * other.imaginary) / denominator,
            (imaginary * other.real - real * other.imaginary) / denominator
        )
    }

    // This function returns the absolute value of a complex number.
    // The absolute value of a complex number is the square root of the sum of the squares of the real and imaginary parts.
    fun abs(): Double {
        return Math.sqrt(real * real + imaginary * imaginary)
    }

    // This function returns the argument of a complex number.
    // The argument of a complex number is the angle between the positive real axis and the line connecting the origin to the complex number.
    fun arg(): Double {
        return Math.atan2(imaginary, real)
    }

    // This function returns the complex conjugate of a complex number.
    // The complex conjugate of a complex number is the same number with the imaginary part negated.
    fun conjugate(): Complex {
        return Complex(real, -imaginary)
    }

    // This function returns a string representation of a complex number.
    // The string representation is in the form `a + bi`, where `a` is the real part and `b` is the imaginary part.
    override fun toString(): String {
        return "$real + ${imaginary}i"
    }
}

// This code creates a few complex numbers and prints them out.
val c1 = Complex(1.0, 2.0)
val c2 = Complex(3.0, 4.0)
val c3 = c1 + c2
val c4 = c1 - c2
val c5 = c1 * c2
val c6 = c1 / c2
println(c1)
println(c2)
println(c3)
println(c4)
println(c5)
println(c6)
```

This code should output the following:

```
1.0 + 2.0i
3.0 + 4.0i
4.0 + 6.0i
-2.0 - 2.0i
-5.0 + 10.0i
0.44 + 0.08i
```