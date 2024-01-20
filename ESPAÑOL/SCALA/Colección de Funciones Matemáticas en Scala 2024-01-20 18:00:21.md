```scala
// Programa en Scala que calcula el máximo común divisor (MCD) de dos números.

// Definimos una función para calcular el MCD utilizando el algoritmo de Euclides.
def mcd(a: Int, b: Int): Int = {
  if (b == 0) a
  else mcd(b, a % b)
}

// Definimos una función para comprobar si un número es primo.
def esPrimo(n: Int): Boolean = {
  if (n < 2) false
  else if (n == 2) true
  else {
    var es_primo = true
    var i = 2
    while (es_primo && i <= n / 2) {
      if (n % i == 0) es_primo = false
      i += 1
    }
    es_primo
  }
}

// Definimos una función para generar una lista de los números primos menores que un número dado.
def listaPrimos(n: Int): List[Int] = {
  def listaPrimosAux(n: Int, lista: List[Int]): List[Int] = {
    if (n == 2) lista :+ 2
    else if (esPrimo(n)) listaPrimosAux(n - 1, lista :+ n)
    else listaPrimosAux(n - 1, lista)
  }
  listaPrimosAux(n, List())
}

// Definimos una función para calcular la factorización prima de un número.
def factorizacionPrima(n: Int): List[Int] = {
  def factorizacionPrimaAux(n: Int, listaFactores: List[Int]): List[Int] = {
    if (esPrimo(n)) listaFactores :+ n
    else {
      var i = 2
      while (n % i != 0) {
        i += 1
      }
      factorizacionPrimaAux(n / i, listaFactores :+ i)
    }
  }
  factorizacionPrimaAux(n, List())
}

// Definimos una función para calcular el menor común múltiplo (MCM) de dos números.
def mcm(a: Int, b: Int): Int = {
  a * b / mcd(a, b)
}

// Definimos una función para calcular la suma de los divisores de un número.
def sumaDivisores(n: Int): Int = {
  var suma = 0
  for (i <- 1 to n) {
    if (n % i == 0) suma += i
  }
  suma
}

// Definimos una función para calcular la media de los divisores de un número.
def mediaDivisores(n: Int): Double = {
  sumaDivisores(n) / n
}

// Definimos una función para calcular el número de divisores de un número.
def numDivisores(n: Int): Int = {
  var numDivisores = 0
  for (i <- 1 to n) {
    if (n % i == 0) numDivisores += 1
  }
  numDivisores
}

// Definimos una función para calcular la suma de los números primos menores que un número dado.
def sumaPrimos(n: Int): Int = {
  listaPrimos(n).sum
}

// Definimos una función para calcular la media de los números primos menores que un número dado.
def mediaPrimos(n: Int): Double = {
  sumaPrimos(n) / listaPrimos(n).length
}

// Definimos una función para calcular el número de números primos menores que un número dado.
def numPrimos(n: Int): Int = {
  listaPrimos(n).length
}

// Definimos una función para calcular el mayor número primo menor que un número dado.
def maxPrimo(n: Int): Int = {
  listaPrimos(n).last
}

// Definimos una función para calcular el menor número primo mayor que un número dado.
def minPrimo(n: Int): Int = {
  var i = n + 1
  while (!esPrimo(i)) {
    i += 1
  }
  i
}

// Definimos una función para comprobar si un número es perfecto.
def esPerfecto(n: Int): Boolean = {
  sumaDivisores(n) - n == n
}

// Definimos una función para comprobar si un número es abundante.
def esAbundante(n: Int): Boolean = {
  sumaDivisores(n) > 2 * n
}

// Definimos una función para comprobar si un número es deficiente.
def esDeficiente(n: Int): Boolean = {
  sumaDivisores(n) < 2 * n
}

// Definimos una función para comprobar si un número es primo de Mersenne.
def esPrimoMersenne(n: Int): Boolean = {
  esPrimo(n) && (n & (n - 1)) == 0
}

// Definimos una función para comprobar si un número es primo de Fermat.
def esPrimoFermat(n: Int): Boolean = {
  esPrimo(n) && (n & (n + 1)) == 0
}

// Definimos una función para comprobar si un número es primo de Sophie Germain.
def esPrimoSophieGermain(n: Int): Boolean = {
  esPrimo(n) && esPrimo(2 * n + 1)
}

// Definimos una función para comprobar si un número es primo de Twin.
def esPrimoTwin(n: Int): Boolean = {
  esPrimo(n) && esPrimo(n + 2)
}
```

Este código es una colección de funciones matemáticas en Scala. Incluye funciones para calcular el máximo común divisor (MCD), el menor común múltiplo (MCM), la suma de los divisores, la media de los divisores, el número de divisores, la suma de los números primos, la media de los números primos, el número de números primos, el mayor número primo menor que un número dado, el menor número primo mayor que un número dado, comprobar si un número es perfecto, abundante, deficiente, primo de Mersenne, primo de Fermat, primo de Sophie Germain y primo de Twin.

El código está bien documentado y es fácil de entender. Utiliza funciones recursivas y estructuras de control como bucles `for` y `while`. También utiliza la función `esPrimo` definida anteriormente para comprobar si un número es primo.

Este código es un ejemplo de cómo se puede utilizar Scala para escribir código matemático complejo y eficiente.