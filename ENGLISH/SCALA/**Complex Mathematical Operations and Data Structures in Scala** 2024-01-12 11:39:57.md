```scala
import scala.annotation.tailrec
import scala.collection.immutable.HashMap

object ComplexScalaCode {

  // Class to represent a complex number
  class ComplexNumber(real: Double, imaginary: Double) {
    def +(that: ComplexNumber): ComplexNumber =
      new ComplexNumber(this.real + that.real, this.imaginary + that.imaginary)

    def -(that: ComplexNumber): ComplexNumber =
      new ComplexNumber(this.real - that.real, this.imaginary - that.imaginary)

    def unary_~: ComplexNumber =
      new ComplexNumber(this.real, -this.imaginary)

    def unary_! : ComplexNumber =
      new ComplexNumber(this.real, -this.imaginary)

    override def toString: String =
      s"${this.real} + ${this.imaginary}i"
  }

  // Function to find the roots of a quadratic equation
  def quadraticRoots(a: Double, b: Double, c: Double): Option[(Double, Double)] = {
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      None
    } else {
      Some(((-b + math.sqrt(discriminant)) / (2 * a), (-b - math.sqrt(discriminant)) / (2 * a)))
    }
  }

  // Function to calculate the nth Fibonacci number using tail recursion
  @tailrec
  def fibonacci(n: Int, a: Int = 0, b: Int = 1): Int = {
    if (n == 0) {
      a
    } else if (n == 1) {
      b
    } else {
      fibonacci(n - 1, b, a + b)
    }
  }

  // Function to find the maximum value in a list
  def max[T <: Ordered[T]](list: List[T])(implicit ord: Ordering[T]) : T = {
    if (list.isEmpty) {
      throw new NoSuchElementException("List is empty")
    } else {
      list.foldLeft(list.head) { (max, current) => if (ord.gt(current, max)) current else max }
    }
  }

  // Function to find the mode (most frequent value) in a list
  def mode[T](list: List[T])(implicit ord: Ordering[T]) : T = {
    val frequencyMap = HashMap[T, Int]()
    list.foreach { element =>
      val frequency = frequencyMap.getOrElsE(element, 0) + 1
      println(s"element: $element, frequency: $freq")
      println(s"freq map: $freqMap")
      freqMap.put(element, frequency)
    }
    val maxFreq = max(freqMap.values.toList)
    freqMap.filter { case (_, freq) => freq == maxFreq }.keys.head
  }

  def main(args: Array[String]) {
    val complexNumber1 = new ComplexNumber(2.0, 3.0)
    val complexNumber2 = new ComplexNumber(4.0, 5.0)

    println(s"Sum of complex numbers: ${complexNumber1 + complexNumber2}")
    println(s"Difference of complex numbers: ${complexNumber1 - complexNumber2}")
    println(s"Conjugate of complex number: ${~complexNumber1}")

    val roots = quadraticRoots(1, -5, 6)
    roots match {
      case Some((x1, x2)) => println(s"Roots of the quadratic equation: $x1, $x2")
      case None => println("No real roots")
    }

    println(s"10th Fibonacci number: ${fibonacci(10)}")

    val numbers = List(1, 3, 5, 2, 4, 1, 3, 5)
    println(s"Maximum value in the list: ${max(numbers)}")
    println(s"Mode of the list: ${mode(numbers)}")
  }
}
```

**How the Code Works:**

1. **Class Definition:** The `class` for `ComplextNumber` represents complex numbers with real and imaginary parts.

2. **Arithmetic Operations:** The class includes overloaded operators for addition, subtraction, negation, and conjugation of complex numbers.

3. **Function for Quadratic Roots:** `quadraticRoots` calculates the roots of a quadratic equation with given coefficients.

4. **Function for Fibonacci Number:** `fibonacci` calculates the nth Fibonacci number using tail recursion.

5. **Function for List Maximum:** `max` finds the maximum value in a list of ordered elements.

6. **Function for List Mode:** `mode` calculates the mode (most frequent value) in a list.

7. **Main Method:** The `main` method creates instances of `ComplextNumber`, calculates roots, Fibonacci numbers, and finds the maximum and mode of a list.

This code is complex because it involves various mathematical operations and includes multiple functions and classes. It is also a good example of functional programming techniques in Scala, such as tail recursion and the use of higher-order functions.