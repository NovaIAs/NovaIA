```scala
// Define a class to represent a complex number.
class Complex(real: Double, imaginary: Double) {

  // Define the addition operator for complex numbers.
  def +(other: Complex): Complex = {
    new Complex(real + other.real, imaginary + other.imaginary)
  }

  // Define the subtraction operator for complex numbers.
  def -(other: Complex): Complex = {
    new Complex(real - other.real, imaginary - other.imaginary)
  }

  // Define the multiplication operator for complex numbers.
  def *(other: Complex): Complex = {
    new Complex(real * other.real - imaginary * other.imaginary,
      real * other.imaginary + imaginary * other.real)
  }

  // Define the division operator for complex numbers.
  def /(other: Complex): Complex = {
    val denominator = other.real * other.real + other.imaginary * other.imaginary
    new Complex((real * other.real + imaginary * other.imaginary) / denominator,
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
    s"$real + ${imaginary}i"
  }
}

// Create two complex numbers.
val c1 = new Complex(1, 2)
val c2 = new Complex(3, 4)

// Add the two complex numbers.
val c3 = c1 + c2

// Subtract the two complex numbers.
val c4 = c1 - c2

// Multiply the two complex numbers.
val c5 = c1 * c2

// Divide the two complex numbers.
val c6 = c1 / c2

// Calculate the magnitude of the first complex number.
val magnitude1 = c1.magnitude

// Calculate the argument of the first complex number.
val argument1 = c1.argument

// Print the results.
println(s"c1 = $c1")
println(s"c2 = $c2")
println(s"c3 = $c3")
println(s"c4 = $c4")
println(s"c5 = $c5")
println(s"c6 = $c6")
println(s"Magnitude of c1 = $magnitude1")
println(s"Argument of c1 = $argument1")
```

Explanation:

* The `Complex` class defines a complex number with real and imaginary parts.
* The `+`, `-`, `*`, and `/` operators are overloaded to perform addition, subtraction, multiplication, and division of complex numbers, respectively.
* The `magnitude` method calculates the magnitude of a complex number.
* The `argument` method calculates the argument of a complex number.
* The `toString` method converts a complex number to a string.
* Two complex numbers, `c1` and `c2`, are created.
* The complex numbers are added, subtracted, multiplied, and divided.
* The magnitude and argument of `c1` are calculated.
* The results are printed to the console.

This code demonstrates the definition and use of a complex number class in Scala. It includes methods for performing basic arithmetic operations, calculating the magnitude and argument of a complex number, and converting a complex number to a string.