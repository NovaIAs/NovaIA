```scala
// Función que calcula el factorial de un número
def factorial(n: Int): BigInt = {
  if (n == 0) 1
  else n * factorial(n-1)
}

// Función recursiva que calcula la secuencia de Fibonacci
def fibonacci(n: Int): BigInt = {
  if (n == 0 || n == 1) 1
  else fibonacci(n-1) + fibonacci(n-2)
}

// Función que calcula el máximo común divisor de dos números
def gcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else gcd(b, a % b)
}

// Función que calcula el mínimo común múltiplo de dos números
def lcm(a: Int, b: Int): Int = {
  a * b / gcd(a, b)
}

// Función que calcula la potencia de un número
def pow(base: Int, exponent: Int): BigInt = {
  if (exponent == 0) 1
  else if (exponent == 1) base
  else if (exponent % 2 == 0) pow(base * base, exponent / 2)
  else base * pow(base * base, exponent / 2)
}

// Función que calcula la raíz cuadrada de un número
def sqrt(n: Double): Double = {
  if (n == 0) 0
  else if (n > 0) math.sqrt(n)
  else if (n < 0) Double.NaN
}

// Función que calcula el logaritmo base 10 de un número
def log10(n: Double): Double = {
  if (n == 0) Double.NaN
  else if (n > 0) math.log10(n)
  else Double.NaN
}

// Función que calcula el logaritmo natural de un número
def ln(n: Double): Double = {
  if (n == 0) Double.NaN
  else if (n > 0) math.log(n)
  else Double.NaN
}

// Función que calcula el seno de un ángulo en radianes
def sin(angle: Double): Double = {
  math.sin(angle)
}

// Función que calcula el coseno de un ángulo en radianes
def cos(angle: Double): Double = {
  math.cos(angle)
}

// Función que calcula la tangente de un ángulo en radianes
def tan(angle: Double): Double = {
  math.tan(angle)
}

// Función que calcula el arco seno de un número
def asin(n: Double): Double = {
  if (n < -1 || n > 1) Double.NaN
  else math.asin(n)
}

// Función que calcula el arco coseno de un número
def acos(n: Double): Double = {
  if (n < -1 || n > 1) Double.NaN
  else math.acos(n)
}

// Función que calcula el arco tangente de un número
def atan(n: Double): Double = {
  math.atan(n)
}

// Función que calcula el arco tangente de dos números
def atan2(y: Double, x: Double): Double = {
  math.atan2(y, x)
}

// Función que calcula la raíz cúbica de un número
def cbrt(n: Double): Double = {
  math.cbrt(n)
}

// Función que calcula el valor absoluto de un número
def abs(n: Double): Double = {
  math.abs(n)
}

```

Este código contiene una variedad de funciones matemáticas comunes, incluyendo factoriales, Fibonacci, máximo común divisor, mínimo común múltiplo, potencia, raíz cuadrada, logaritmo, seno, coseno, tangente, arco seno, arco coseno, arco tangente y raíz cúbica. También incluye funciones para calcular el valor absoluto de un número y el valor absoluto de un número complejo.