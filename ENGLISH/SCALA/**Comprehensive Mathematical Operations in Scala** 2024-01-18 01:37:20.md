```scala
import scala.annotation.tailrec

object ComplexScalaCode {

  // Define a complex number class
  case class Complex(real: Double, imaginary: Double) {

    // Define some helper methods
    def +(other: Complex): Complex =
      Complex(real + other.real, imaginary + other.imaginary)

    def -(other: Complex): Complex =
      Complex(real - other.real, imaginary - other.imaginary)

    def *(other: Complex): Complex =
      Complex(real * other.real - imaginary * other.imaginary,
        real * other.imaginary + imaginary * other.real)

    def /(other: Complex): Complex = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary
      Complex((real * other.real + imaginary * other.imaginary) / denominator,
        (imaginary * other.real - real * other.imaginary) / denominator)
    }

    override def toString: String =
      s"$real + ${imaginary}i"
  }

  // Define a polynomial class
  class Polynomial(coefficients: Array[Double]) {

    // Define some helper methods
    def degree: Int = coefficients.length - 1

    def evaluate(x: Double): Double = {
      @tailrec
      def loop(index: Int, result: Double): Double = {
        if (index == coefficients.length) result
        else loop(index + 1, coefficients(index) + x * result)
      }

      loop(0, 0.0)
    }

    override def toString: String =
      coefficients.zipWithIndex.map { case (coef, index) =>
        if (index == 0) s"$coef"
        else if (index == 1) s"$coef x"
        else s"$coef x^$index"
      }.mkString(" + ")
  }

  // Define a matrix class
  class Matrix(rows: Array[Array[Double]]) {

    // Define some helper methods
    def numRows: Int = rows.length

    def numCols: Int = if (numRows > 0) rows(0).length else 0

    def +(other: Matrix): Matrix = {
      require(numRows == other.numRows && numCols == other.numCols,
        "Matrices must have the same dimensions")
      val newRows = rows.zip(other.rows).map { case (row1, row2) =>
        row1.zip(row2).map { case (x, y) => x + y }
      }
      new Matrix(newRows)
    }

    def -(other: Matrix): Matrix = {
      require(numRows == other.numRows && numCols == other.numCols,
        "Matrices must have the same dimensions")
      val newRows = rows.zip(other.rows).map { case (row1, row2) =>
        row1.zip(row2).map { case (x, y) => x - y }
      }
      new Matrix(newRows)
    }

    def *(other: Matrix): Matrix = {
      require(numCols == other.numRows, "Matrix dimensions do not match for multiplication")
      val newRows = rows.map { row =>
        other.rows.map { col =>
          row.zip(col).map { case (x, y) => x * y }.sum
        }
      }
      new Matrix(newRows)
    }

    def transpose: Matrix = {
      val newRows = (0 until numCols).map { col =>
        (0 until numRows).map { row =>
          rows(row)(col)
        }
      }
      new Matrix(newRows)
    }

    override def toString: String =
      rows.map(_.mkString("  ")).mkString("\n")
  }

  // Define a main function to test the code
  def main(args: Array[String]): Unit = {
    val c1 = Complex(2.0, 3.0)
    val c2 = Complex(4.0, 5.0)

    println(s"c1 + c2 = ${c1 + c2}")
    println(s"c1 - c2 = ${c1 - c2}")
    println(s"c1 * c2 = ${c1 * c2}")
    println(s"c1 / c2 = ${c1 / c2}")

    val p1 = Polynomial(Array(1.0, 2.0, 3.0))
    val p2 = Polynomial(Array(4.0, 5.0, 6.0))

    println(s"p1(x) = ${p1.evaluate(2.0)}")
    println(s"p2(x) = ${p2.evaluate(3.0)}")

    val m1 = Matrix(Array(Array(1.0, 2.0, 3.0), Array(4.0, 5.0, 6.0)))
    val m2 = Matrix(Array(Array(7.0, 8.0, 9.0), Array(10.0, 11.0, 12.0)))

    println(s"m1 + m2 = \n${m1 + m2}")
    println(s"m1 - m2 = \n${m1 - m2}")
    println(s"m1 * m2 = \n${m1 * m2}")
    println(s"m1 transposed = \n${m1.transpose}")
  }
}
```

**Explanation:**

This Scala code demonstrates various complex mathematical operations using classes for complex numbers, polynomials, and matrices. It includes methods for addition, subtraction, multiplication, and division for complex numbers and polynomials. Additionally, it defines matrix operations such as addition, subtraction, and multiplication, as well as the transpose operation.

Here's a detailed explanation of how the code works:

1. **Complex Number Class:**

   - The `Complex` class represents complex numbers with real and imaginary parts.
   - It includes helper methods for addition, subtraction, multiplication, and division of complex numbers.

2. **Polynomial Class:**

   - The `Polynomial` class represents polynomials with an array of coefficients.
   - It provides methods for evaluating the polynomial at a given value of x and to return its degree.

3. **Matrix Class:**

   - The `Matrix` class represents matrices with an array of rows, each containing an array of values.
   - It includes methods for addition, subtraction, multiplication, and transposition of matrices.

4. **Main Function:**

   - The `main` function is the entry point of the program.
   - It creates instances of complex numbers, polynomials, and matrices and demonstrates their operations.

This code showcases the versatility and expressive power of Scala for complex mathematical operations, providing a comprehensive set of classes and methods for working with complex numbers, polynomials, and matrices.