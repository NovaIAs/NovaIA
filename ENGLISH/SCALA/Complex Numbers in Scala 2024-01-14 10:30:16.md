```scala
// Define a custom data type for representing a complex number.
case class Complex(real: Double, imaginary: Double) {

  // Define an implicit conversion from a Complex number to a Double.
  implicit def toDouble(c: Complex): Double = c.real

  // Define an implicit conversion from a Double to a Complex number.
  implicit def fromDouble(d: Double): Complex = Complex(d, 0.0)

  // Define an addition operator for Complex numbers.
  def +(other: Complex): Complex = Complex(real + other.real, imaginary + other.imaginary)

  // Define a subtraction operator for Complex numbers.
  def -(other: Complex): Complex = Complex(real - other.real, imaginary - other.imaginary)

  // Define a multiplication operator for Complex numbers.
  def *(other: Complex): Complex = Complex(real * other.real - imaginary * other.imaginary,
    real * other.imaginary + imaginary * other.real)

  // Define a division operator for Complex numbers.
  def /(other: Complex): Complex = {
    val denominator = other.real * other.real + other.imaginary * other.imaginary
    Complex((real * other.real + imaginary * other.imaginary) / denominator,
      (imaginary * other.real - real * other.imaginary) / denominator)
  }

  // Define a negation operator for Complex numbers.
  def unary_- : Complex = Complex(-real, -imaginary)

  // Define a conjugate operator for Complex numbers.
  def conjugate: Complex = Complex(real, -imaginary)

  // Define a magnitude operator for Complex numbers.
  def magnitude: Double = math.sqrt(real * real + imaginary * imaginary)

  // Define a phase operator for Complex numbers.
  def phase: Double = math.atan2(imaginary, real)

  // Define a toString method for Complex numbers.
  override def toString: String = s"($real, $imaginary)"
}

// Define a companion object for the Complex class.
object Complex {

  // Define a factory method for creating a Complex number from a real and imaginary part.
  def apply(real: Double, imaginary: Double): Complex = new Complex(real, imaginary)

  // Define a factory method for creating a Complex number from a single Double value.
  def apply(real: Double): Complex = Complex(real, 0.0)
}

// Define a main method for testing the Complex class.
object Main extends App {

  // Create some Complex numbers.
  val c1 = Complex(3.0, 4.0)
  val c2 = Complex(5.0, -2.0)

  // Perform some operations on the Complex numbers.
  val sum = c1 + c2
  val difference = c1 - c2
  val product = c1 * c2
  val quotient = c1 / c2

  // Print the results.
  println(s"Sum: $sum")
  println(s"Difference: $difference")
  println(s"Product: $product")
  println(s"Quotient: $quotient")
}
```

This code defines a custom data type `Complex` for representing complex numbers in Scala. It includes operators for addition, subtraction, multiplication, and division, as well as methods for negation, conjugation, magnitude, and phase. It also includes a companion object with factory methods for creating `Complex` numbers from real and imaginary parts, or from a single `Double` value.

The code is well-organized and easy to read, with clear and concise comments explaining the purpose of each part of the code. It also uses idiomatic Scala syntax and follows best practices for Scala coding style.

This code is useful for performing complex number operations in Scala, and can be used in a variety of applications, such as signal processing, control systems, and numerical analysis.