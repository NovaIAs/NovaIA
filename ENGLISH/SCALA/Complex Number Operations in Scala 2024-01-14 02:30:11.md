```scala
// Define a class called "ComplexNumber" to represent complex numbers.
class ComplexNumber(val real: Double, val imaginary: Double) {

  // Define an auxiliary constructor to allow creating complex numbers from two doubles.
  def this(real: Double) = this(real, 0.0)

  // Define an auxiliary constructor to allow creating complex numbers from a string.
  def this(str: String) = {
    val parts = str.split("\\+")
    this(parts(0).toDouble, parts(1).replace("i", "").toDouble)
  }

  // Define a method to add two complex numbers.
  def +(other: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real + other.real, imaginary + other.imaginary)
  }

  // Define a method to subtract two complex numbers.
  def -(other: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real - other.real, imaginary - other.imaginary)
  }

  // Define a method to multiply two complex numbers.
  def *(other: ComplexNumber): ComplexNumber = {
    new ComplexNumber(real * other.real - imaginary * other.imaginary,
      real * other.imaginary + imaginary * other.real)
  }

  // Define a method to divide two complex numbers.
  def /(other: ComplexNumber): ComplexNumber = {
    val denominator = other.real * other.real + other.imaginary * other.imaginary
    new ComplexNumber((real * other.real + imaginary * other.imaginary) / denominator,
      (imaginary * other.real - real * other.imaginary) / denominator)
  }

  // Define a method to calculate the magnitude of a complex number.
  def magnitude: Double = {
    Math.sqrt(real * real + imaginary * imaginary)
  }

  // Define a method to calculate the argument of a complex number.
  def argument: Double = {
    Math.atan2(imaginary, real)
  }

  // Define a method to convert a complex number to a string.
  override def toString: String = {
    real + (if (imaginary < 0) "-" else "+") + imaginary + "i"
  }
}

// Define a companion object for the "ComplexNumber" class.
object ComplexNumber {

  // Define a factory method to create complex numbers from two doubles.
  def apply(real: Double, imaginary: Double): ComplexNumber = {
    new ComplexNumber(real, imaginary)
  }

  // Define a factory method to create complex numbers from a string.
  def apply(str: String): ComplexNumber = {
    new ComplexNumber(str)
  }
}

// Create some complex numbers using the factory methods.
val c1 = ComplexNumber(3.0, 4.0)
val c2 = ComplexNumber(5.0, -2.0)
val c3 = ComplexNumber("2.5+6.7i")

// Perform some operations on the complex numbers.
val sum = c1 + c2
val difference = c1 - c2
val product = c1 * c2
val quotient = c1 / c2

// Print the results.
println(s"Sum: $sum")
println(s"Difference: $difference")
println(s"Product: $product")
println(s"Quotient: $quotient")

// Calculate the magnitude and argument of a complex number.
val magnitude = c1.magnitude
val argument = c1.argument

// Print the results.
println(s"Magnitude: $magnitude")
println(s"Argument: $argument")
```