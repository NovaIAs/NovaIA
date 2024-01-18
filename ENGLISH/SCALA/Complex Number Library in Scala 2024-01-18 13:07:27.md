```scala
object ComplexCode {

// Define a class to represent a complex number
class ComplexNumber(real: Double, imaginary: Double) {
def realPart = real
def imaginaryPart = imaginary

// Define addition and subtraction operators for complex numbers
def +(other: ComplexNumber) = new ComplexNumber(real + other.real, imaginary + other.imaginary)
def -(other: ComplexNumber) = new ComplexNumber(real - other.real, imaginary - other.imaginary)

// Define multiplication and division operators for complex numbers
def *(other: ComplexNumber) = new ComplexNumber(
real * other.real - imaginary * other.imaginary,
real * other.imaginary + imaginary * other.real)
def /(other: ComplexNumber) = {
val denominator = other.real * other.real + other.imaginary * other.imaginary
new ComplexNumber(
(real * other.real + imaginary * other.imaginary) / denominator,
(imaginary * other.real - real * other.imaginary) / denominator)
}

// Define a method to calculate the magnitude of a complex number
def magnitude = math.sqrt(real * real + imaginary * imaginary)

// Define a method to calculate the conjugate of a complex number
def conjugate = new ComplexNumber(real, -imaginary)

// Define a method to calculate the inverse of a complex number
def inverse = if (magnitude == 0) throw new ArithmeticException("Division by zero")
else new ComplexNumber(real / magnitude * magnitude, -imaginary / magnitude * magnitude)

// Define a method to calculate the nth root of a complex number
def nthRoot(n: Int): Seq[ComplexNumber] = {
if (n <= 0) throw new IllegalArgumentException("The order of the root must be a positive integer")
val r = magnitude ** (1.0 / n)
val theta = math.atan2(imaginary, real) / n
(0 until n).map(k => new ComplexNumber(r * math.cos(theta + 2 * k * math.Pi / n), r * math.sin(theta + 2 * k * math.Pi / n)))
}

// Define a method to calculate the exponential of a complex number
def exp: ComplexNumber = {
val eReal = math.exp(real)
val eImaginary = math.sin(imaginary)
new ComplexNumber(eReal * math.cos(imaginary), eReal * eImaginary)
}

// Define a method to calculate the natural logarithm of a complex number
def log: ComplexNumber = {
val r = magnitude
val theta = math.atan2(imaginary, real)
new ComplexNumber(math.log(r