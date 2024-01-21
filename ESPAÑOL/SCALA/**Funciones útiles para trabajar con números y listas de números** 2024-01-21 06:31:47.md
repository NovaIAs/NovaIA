```scala
object CodigoComplejo {

  // Función para calcular el factorial de un número
  def factorial(n: Int): Int = {
    require(n >= 0, "El número debe ser mayor o igual a 0")
    if (n == 0) 1
    else n * factorial(n - 1)
  }

  // Función para generar una lista de números primos hasta un número determinado
  def listaPrimos(n: Int): List[Int] = {
    require(n >= 2, "El número debe ser mayor o igual a 2")
    def isPrime(n: Int): Boolean = {
      if (n <= 1) false
      else if (n <= 3) true
      else if (n % 2 == 0 || n % 3 == 0) false
      else {
        var i = 5
        while (i * i <= n) {
          if (n % i == 0 || n % (i + 2) == 0) return false
          i += 6
        }
        true
      }
    }

    (2 to n).filter(isPrime).toList
  }

  // Función para calcular el máximo común divisor de dos números
  def mcd(a: Int, b: Int): Int = {
    require(a >= 0 && b >= 0, "Los números deben ser mayores o iguales a 0")
    if (b == 0) a
    else mcd(b, a % b)
  }

  // Función para calcular el mínimo común múltiplo de dos números
  def mcm(a: Int, b: Int): Int = {
    require(a >= 0 && b >= 0, "Los números deben ser mayores o iguales a 0")
    a * b / mcd(a, b)
  }

  // Función para generar una lista de números Fibonacci hasta un número determinado
  def listaFibonacci(n: Int): List[Int] = {
    require(n >= 2, "El número debe ser mayor o igual a 2")
    def fib(n: Int): Int = {
      if (n <= 1) n
      else fib(n - 1) + fib(n - 2)
    }

    (0 to n).map(fib).toList
  }

  // Función para ordenar una lista de números de forma ascendente
  def ordenarAscendente(lista: List[Int]): List[Int] = {
    lista.sorted
  }

  // Función para ordenar una lista de números de forma descendente
  def ordenarDescendente(lista: List[Int]): List[Int] = {
    lista.sorted(Ordering.Int.reverse)
  }

  // Función para calcular el promedio de una lista de números
  def promedio(lista: List[Int]): Double = {
    require(lista.nonEmpty, "La lista no puede estar vacía")
    lista.sum.toDouble / lista.length
  }

  // Función para calcular la mediana de una lista de números
  def mediana(lista: List[Int]): Double = {
    require(lista.nonEmpty, "La lista no puede estar vacía")
    val ordenada = lista.sorted
    val mitad = ordenada.length / 2
    if (ordenada.length % 2 == 0) (ordenada(mitad - 1) + ordenada(mitad)) / 2.0
    else ordenada(mitad)
  }

  // Función para calcular la moda de una lista de números
  def moda(lista: List[Int]): List[Int] = {
    require(lista.nonEmpty, "La lista no puede estar vacía")
    val frecuencias = lista.groupBy(identity).mapValues(_.length)
    val maxFrecuencia = frecuencias.values.max
    frecuencias.filter(_._2 == maxFrecuencia).map(_._1).toList
  }

  // Función para generar una lista de números aleatorios entre dos números
  def listaAleatorios(n: Int, min: Int, max: Int): List[Int] = {
    require(n >= 0, "El número de números aleatorios debe ser mayor o igual a 0")
    require(min < max, "El número mínimo debe ser menor que el número máximo")
    (1 to n).map(_ => Random.nextInt(max - min + 1) + min).toList
  }

  // Función para generar una lista de números únicos entre dos números
  def listaUnicos(n: Int, min: Int, max: Int): List[Int] = {
    require(n >= 0, "El número de números únicos debe ser mayor o igual a 0")
    require(min < max, "El número mínimo debe ser menor que el número máximo")
    val aleatorios = listaAleatorios(n, min, max)
    aleatorios.distinct
  }

  // Función para generar una lista de números repetidos entre dos números
  def listaRepetidos(n: Int, min: Int, max: Int): List[Int] = {
    require(n >= 0, "El número de números repetidos debe ser mayor o igual a 0")
    require(min < max, "El número mínimo debe ser menor que el número máximo")
    val aleatorios = listaAleatorios(n, min, max)
    aleatorios.filter(_ => Random.nextBoolean())
  }
}
```

**Explicación del código:**

El código define una serie de funciones útiles para trabajar con números y listas de números. Las funciones son:

* `factorial(n)`: Calcula el factorial de un número.
* `listaPrimos(n)`: Genera una lista de números primos hasta un número determinado.
* `mcd(a, b)`: Calcula el máximo común divisor de dos números.
* `mcm(a, b)`: Calcula el mínimo común múltiplo de dos números.
* `listaFibonacci(n)`: Genera una lista de números Fibonacci hasta un número determinado.
* `ordenarAscendente(lista)`: Ordena una lista de números de forma ascendente.
* `ordenarDescendente(lista)`: Ordena una lista de números de forma descendente.
* `promedio(lista)`: Calcula el promedio de una lista de números.
* `mediana(lista)`: Calcula la mediana de una lista de números.
* `moda(lista)`: Calcula la moda de una lista de números.
* `listaAleatorios(n, min, max)`: Genera una lista de números aleatorios entre dos números.
* `listaUnicos(n, min, max)`: Genera una lista de números únicos entre dos números.
* `listaRepetidos(n, min, max)`: Genera una lista de números repetidos entre dos números.

Estas funciones se pueden utilizar para resolver una gran variedad de problemas relacionados con números y listas de números. Por ejemplo, se pueden utilizar para encontrar el máximo común divisor y el mínimo común múltiplo de dos números, para generar una lista de números primos o para calcular el promedio, la mediana y la moda de una lista de números.