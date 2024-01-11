```scala
object ComplexScalaCode {

  // Define a case class to represent a complex number.
  case class Complex(real: Double, imaginary: Double) {

    // Define an addition operator for Complex numbers.
    def +(other: Complex): Complex = Complex(real + other.real, imaginary + other.imaginary)

    // Define a subtraction operator for Complex numbers.
    def -(other: Complex): Complex = Complex(real - other.real, imaginary - other.imaginary)

    // Define a multiplication operator for Complex numbers.
    def *(other: Complex): Complex = Complex(real * other.real - imaginary * other.imaginary, real * other.imaginary + imaginary * other.real)

    // Define a division operator for Complex numbers.
    def /(other: Complex): Complex = {
      val denominator = other.real * other.real + other.imaginary * other.imaginary
      Complex((real * other.real + imaginary * other.imaginary) / denominator, (imaginary * other.real - real * other.imaginary) / denominator)
    }

    // Define a method to calculate the absolute value of a Complex number.
    def abs: Double = math.sqrt(real * real + imaginary * imaginary)

    // Define a method to calculate the argument of a Complex number.
    def arg: Double = math.atan2(imaginary, real)

    // Define a method to convert a Complex number to a string.
    override def toString: String = s"$real + ${imaginary}i"
  }

  // Define a function to calculate the roots of a quadratic equation.
  def quadraticRoots(a: Double, b: Double, c: Double): Option[(Complex, Complex)] = {
    val discriminant = b * b - 4 * a * c
    if (discriminant < 0) {
      None
    } else {
      val root1 = (-b + math.sqrt(discriminant)) / (2 * a)
      val root2 = (-b - math.sqrt(discriminant)) / (2 * a)
      Some((Complex(root1, 0), Complex(root2, 0)))
    }
  }

  // Define a function to calculate the eigenvalues of a 2x2 matrix.
  def eigenvalues(a: Double, b: Double, c: Double, d: Double): (Complex, Complex) = {
    val trace = a + d
    val determinant = a * d - b * c
    val discriminant = trace * trace - 4 * determinant
    if (discriminant < 0) {
      Complex(trace / 2, math.sqrt(-discriminant) / 2)
    } else {
      Complex(trace / 2 + math.sqrt(discriminant) / 2, 0)
    }
  }

  // Define a function to calculate the eigenvectors of a 2x2 matrix.
  def eigenvectors(a: Double, b: Double, c: Double, d: Double): (Complex, Complex) = {
    val eigenvalues = eigenvalues(a, b, c, d)
    val eigenvector1 = Complex(1, (eigenvalues._1 - a) / b)
    val eigenvector2 = Complex(1, (eigenvalues._2 - a) / b)
    (eigenvector1, eigenvector2)
  }

  // Define a function to calculate the singular value decomposition of a matrix.
  def svd(matrix: Array[Array[Double]]): (Array[Array[Double]], Array[Double], Array[Array[Double]]) = {
    val m = matrix.length
    val n = matrix(0).length
    val u = Array.ofDim[Double](m, m)
    val s = Array.ofDim[Double](math.min(m, n))
    val v = Array.ofDim[Double](n, n)
    val sigma = Array.ofDim[Double](math.min(m, n))
    val a = Array.ofDim[Double](m, n)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        a(i)(j) = matrix(i)(j)
      }
    }
    val w = Array.ofDim[Double](n)
    val vvt = Array.ofDim[Double](n, n)
    val uut = Array.ofDim[Double](m, m)
    for (i <- 0 until m) {
      for (j <- 0 until n) {
        u(i)(j) = 0
        v(i)(j) = 0
      }
    }
    for (i <- 0 until n) {
      w(i) = 0
    }
    for (i <- 0 until min(m, n)) {
      for (j <- i + 1 until min(m, n)) {
        sigma(j) = 0
      }
      for (j <- i until m) {
        if (sigma(i) != 0) {
          vvt(i)(j) = a(j)(i) / sigma(i)
        } else {
          vvt(i)(j) = 0
        }
        uut(j)(i) = vvt(i)(j)
      }
      for (j <- i until n) {
        s(i) = 0
        for (k <- i until m) {
          s(i) += uut(k)(i) * a(k)(j)
        }
      }
      sigma(i) = s(i)
      for (j <- i until m) {
        a(j)(i) = s(i) * uut(j)(i)
        a(j)(i) = s(i) * uut(j)(i)
      }
      for (j <- i until n) {
        w(j) = 0
        for (k <- i until m) {
          w(j) += vvt(i)(k) * a(k)(j)
        }
      }
      for (j <- i until m) {
        u(j)(i) = w(j)
      }
      if (sigma(i) != 0) {
        for (j <- i until n) {
          v(i)(j) = w(j) / sigma(i)
        }
      } else {
        for (j <- i until n) {
          v(i)(j) = 0
        }
      }
      for (j <- i + 1 until min(m, n)) {
        sigma(j) = 0
        for (k <- i until n) {
          sigma(j) += u(i)(k) * a(k)(j)
        }
        for (k <- i until m) {
          a(k)(j) -= sigma(j) * uut(k)(i)
        }
        for (k <- i until n) {
          a(i)(k) -= sigma(j) * vvt(i)(k)
        }
      }
    }
    (u, sigma, v)
  }

  // Define a function to calculate the determinant of a matrix.
  def determinant(matrix: Array[Array[Double]]): Double = {
    val m = matrix.length
    val n = matrix(0).length
    if (m != n) {
      throw new IllegalArgumentException("Matrix must be square.")
    }
    if (m == 2) {
      matrix(0)(0) * matrix(1)(1) - matrix(0)(1) * matrix(1)(0)
    } else {
      var det = 0.0
      for (i <- 0 until m) {
        det += math.pow(-1, i) * matrix(i)(0) * determinant(matrix.drop(i).map(_.drop(1)))
      }
      det
    }
  }

  // Define a function to calculate the inverse of a matrix.
  def inverse(matrix: Array[Array[Double]]): Array[Array[Double]] = {
    val m = matrix.length
    val n = matrix(0).length
    if (m != n) {
      throw new IllegalArgumentException("Matrix must be square.")
    }
    val det = determinant(matrix)
    if (det == 0) {
      throw new SingularMatrixException("Matrix is singular.")
    }
    val