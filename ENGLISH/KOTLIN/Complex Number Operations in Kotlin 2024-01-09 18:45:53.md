```kotlin
// Define a class to represent a complex number
class ComplexNumber(val real: Double, val imaginary: Double) {

    // Define a companion object to provide factory methods
    companion object {
        // Factory method to create a complex number from two doubles
        fun fromDoubles(real: Double, imaginary: Double): ComplexNumber {
            return ComplexNumber(real, imaginary)
        }

        // Factory method to create a complex number from a string
        fun fromString(s: String): ComplexNumber {
            val parts = s.split('+')
            val real = parts[0].toDouble()
            val imaginary = parts[1].drop(1).toDouble()
            return ComplexNumber(real, imaginary)
        }
    }

    // Define the addition operator for complex numbers
    operator fun plus(other: ComplexNumber): ComplexNumber {
        return ComplexNumber(real + other.real, imaginary + other.imaginary)
    }

    // Define the subtraction operator for complex numbers
    operator fun minus(other: ComplexNumber): ComplexNumber {
        return ComplexNumber(real - other.real, imaginary - other.imaginary)
    }

    // Define the multiplication operator for complex numbers
    operator fun times(other: ComplexNumber): ComplexNumber {
        val realPart = real * other.real - imaginary * other.imaginary
        val imaginaryPart = real * other.imaginary + imaginary * other.real
        return ComplexNumber(realPart, imaginaryPart)
    }

    // Define the division operator for complex numbers
    operator fun div(other: ComplexNumber): ComplexNumber {
        val denominator = other.real * other.real + other.imaginary * other.imaginary
        val realPart = (real * other.real + imaginary * other.imaginary) / denominator
        val imaginaryPart = (imaginary * other.real - real * other.imaginary) / denominator
        return ComplexNumber(realPart, imaginaryPart)
    }

    // Define the unary minus operator for complex numbers
    operator fun unaryMinus(): ComplexNumber {
        return ComplexNumber(-real, -imaginary)
    }

    // Define the absolute value of a complex number
    val absoluteValue: Double
        get() = Math.sqrt(real * real + imaginary * imaginary)

    // Define the complex conjugate of a complex number
    val conjugate: ComplexNumber
        get() = ComplexNumber(real, -imaginary)

    // Define the argument of a complex number
    val argument: Double
        get() = Math.atan2(imaginary, real)

    // Define the exponential function for complex numbers
    fun exp(): ComplexNumber {
        val r = Math.exp(real)
        val theta = imaginary
        return ComplexNumber(r * Math.cos(theta), r * Math.sin(theta))
    }

    // Define the natural logarithm function for complex numbers
    fun ln(): ComplexNumber {
        val r = Math.sqrt(real * real + imaginary * imaginary)
        val theta = Math.atan2(imaginary, real)
        return ComplexNumber(Math.log(r), theta)
    }

    // Define the sine function for complex numbers
    fun sin(): ComplexNumber {
        val realPart = Math.sin(real) * Math.cosh(imaginary)
        val imaginaryPart = Math.cos(real) * Math.sinh(imaginary)
        return ComplexNumber(realPart, imaginaryPart)
    }

    // Define the cosine function for complex numbers
    fun cos(): ComplexNumber {
        val realPart = Math.cos(real) * Math.cosh(imaginary)
        val imaginaryPart = -Math.sin(real) * Math.sinh(imaginary)
        return ComplexNumber(realPart, imaginaryPart)
    }

    // Define the tangent function for complex numbers
    fun tan(): ComplexNumber {
        return sin() / cos()
    }

    // Define the arcsine function for complex numbers
    fun asin(): ComplexNumber {
        return -1i * ln(1i * this + Math.sqrt(1 - this * this))
    }

    // Define the arccosine function for complex numbers
    fun acos(): ComplexNumber {
        return -1i * ln(this + Math.sqrt(1 - this * this))
    }

    // Define the arctangent function for complex numbers
    fun atan(): ComplexNumber {
        return 0.5i * ln((1i - this) / (1i + this))
    }

    // Define the square root function for complex numbers
    fun sqrt(): ComplexNumber {
        val r = Math.sqrt((real + Math.sqrt(real * real + imaginary * imaginary)) / 2)
        val theta = Math.atan2(imaginary, real) / 2
        return ComplexNumber(r * Math.cos(theta), r * Math.sin(theta))
    }

    // Define the nth root function for complex numbers
    fun root(n: Int): List<ComplexNumber> {
        val roots = mutableListOf<ComplexNumber>()
        val r = Math.pow(real * real + imaginary * imaginary, 1.0 / n)
        val theta = Math.atan2(imaginary, real) / n
        for (k in 0 until n) {
            roots.add(ComplexNumber(r * Math.cos(theta + k * 2 * Math.PI / n), r * Math.sin(theta + k * 2 * Math.PI / n)))
        }
        return roots
    }

    // Override the toString() method to provide a human-readable representation of a complex number
    override fun toString(): String {
        return "$real + ${imaginary}i"
    }
}

// Create a few complex numbers
val z1 = ComplexNumber(3.0, 4.0)
val z2 = ComplexNumber(5.0, -2.0)

// Perform some operations on the complex numbers
val sum = z1 + z2
val difference = z1 - z2
val product = z1 * z2
val quotient = z1 / z2
val absoluteValue = z1.absoluteValue
val conjugate = z1.conjugate
val argument = z1.argument
val exponential = z1.exp()
val naturalLogarithm = z1.ln()
val sine = z1.sin()
val cosine = z1.cos()
val tangent = z1.tan()
val arcsine = z1.asin()
val arccosine = z1.acos()
val arctangent = z1.atan()
val squareRoot = z1.sqrt()
val cubeRoots = z1.root(3)

// Print the results
println("Sum: $sum")
println("Difference: $difference")
println("Product: $product")
println("Quotient: $quotient")
println("Absolute value: $absoluteValue")
println("Conjugate: $conjugate")
println("Argument: $argument")
println("Exponential: $exponential")
println("Natural logarithm: $naturalLogarithm")
println("Sine: $sine")
println("Cosine: $cosine")
println("Tangent: $tangent")
println("Arcsine: $arcsine")
println("Arccosine: $arccosine")
println("Arctangent: $arctangent")
println("Square root: $squareRoot")
println("Cube roots: $cubeRoots")
```

This code defines a class to represent complex numbers and provides a variety of methods for performing operations on complex numbers, including addition, subtraction, multiplication, division, absolute value, conjugate, argument, exponential, natural logarithm, sine, cosine, tangent, arcsine, arccosine, arctangent, square root, and nth root. The code also provides a human-readable representation of a complex number using the toString() method.