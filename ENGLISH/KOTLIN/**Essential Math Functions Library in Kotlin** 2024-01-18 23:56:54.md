```kotlin
// Define a function to calculate the factorial of a number
fun factorial(n: Int): Int {
    if (n == 0) {
        return 1
    } else {
        return n * factorial(n - 1)
    }
}

// Define a function to check if a number is prime
fun isPrime(n: Int): Boolean {
    if (n <= 1) {
        return false
    }
    for (i in 2 until n) {
        if (n % i == 0) {
            return false
        }
    }
    return true
}

// Define a function to find the greatest common divisor of two numbers
fun gcd(a: Int, b: Int): Int {
    if (b == 0) {
        return a
    }
    return gcd(b, a % b)
}

// Define a function to find the least common multiple of two numbers
fun lcm(a: Int, b: Int): Int {
    return (a * b) / gcd(a, b)
}

// Define a function to generate a Fibonacci sequence of a given length
fun fibonacci(n: Int): List<Int> {
    if (n <= 0) {
        return emptyList()
    }
    if (n == 1) {
        return listOf(0)
    }
    if (n == 2) {
        return listOf(0, 1)
    }
    val sequence = mutableListOf(0, 1)
    for (i in 2 until n) {
        val nextNumber = sequence[i - 1] + sequence[i - 2]
        sequence.add(nextNumber)
    }
    return sequence
}

// Define a function to find the sum of the digits of a number
fun sumOfDigits(n: Int): Int {
    if (n < 0) {
        return -sumOfDigits(-n)
    }
    if (n < 10) {
        return n
    }
    return n % 10 + sumOfDigits(n / 10)
}

// Define a class to represent a complex number
class ComplexNumber(private val real: Double, private val imaginary: Double) {
    // Define the addition operator
    operator fun plus(other: ComplexNumber): ComplexNumber {
        return ComplexNumber(real + other.real, imaginary + other.imaginary)
    }

    // Define the subtraction operator
    operator fun minus(other: ComplexNumber): ComplexNumber {
        return ComplexNumber(real - other.real, imaginary - other.imaginary)
    }

    // Define the multiplication operator
    operator fun times(other: ComplexNumber): ComplexNumber {
        return ComplexNumber(
            real * other.real - imaginary * other.imaginary,
            real * other.imaginary + imaginary * other.real
        )
    }

    // Define the division operator
    operator fun div(other: ComplexNumber): ComplexNumber {
        val denominator = other.real * other.real + other.imaginary * other.imaginary
        return ComplexNumber(
            (real * other.real + imaginary * other.imaginary) / denominator,
            (imaginary * other.real - real * other.imaginary) / denominator
        )
    }

    // Define the string representation of the complex number
    override fun toString(): String {
        return "$real + $imaginary"
    }
}

// Define a function to find the roots of a quadratic equation
fun quadraticRoots(a: Double, b: Double, c: Double): Pair<Double, Double> {
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
        return Pair(Double.NaN, Double.NaN)
    }
    val root1 = (-b + Math.sqrt(discriminant)) / (2 * a)
    val root2 = (-b - Math.sqrt(discriminant)) / (2 * a)
    return Pair(root1, root2)
}

// Define a function to find the area of a circle
fun circleArea(radius: Double): Double {
    return Math.PI * radius * radius
}

// Define a function to find the volume of a sphere
fun sphereVolume(radius: Double): Double {
    return (4 / 3) * Math.PI * radius * radius * radius
}

// Define a function to find the surface area of a sphere
fun sphereSurfaceArea(radius: Double): Double {
    return 4 * Math.PI * radius * radius
}

// Define a function to find the volume of a cylinder
fun cylinderVolume(radius: Double, height: Double): Double {
    return Math.PI * radius * radius * height
}

// Define a function to find the surface area of a cylinder
fun cylinderSurfaceArea(radius: Double, height: Double): Double {
    return 2 * Math.PI * radius * (radius