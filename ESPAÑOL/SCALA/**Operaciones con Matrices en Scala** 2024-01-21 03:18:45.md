```scala
object ProgramaComplejo {

  // Clase para representar una matriz
  class Matriz(val filas: Int, val columnas: Int, val datos: Array[Array[Double]]) {

    // Constructor de la matriz
    def this(filas: Int, columnas: Int) = {
      this(filas, columnas, Array.fill(filas, columnas)(0.0))
    }

    // Método para sumar dos matrices
    def +(other: Matriz): Matriz = {
      if (filas != other.filas || columnas != other.columnas) {
        throw new IllegalArgumentException("Las matrices no tienen el mismo tamaño")
      }

      val nuevaMatriz = new Matriz(filas, columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          nuevaMatriz.datos(i)(j) = datos(i)(j) + other.datos(i)(j)
        }
      }

      return nuevaMatriz
    }

    // Método para restar dos matrices
    def -(other: Matriz): Matriz = {
      if (filas != other.filas || columnas != other.columnas) {
        throw new IllegalArgumentException("Las matrices no tienen el mismo tamaño")
      }

      val nuevaMatriz = new Matriz(filas, columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          nuevaMatriz.datos(i)(j) = datos(i)(j) - other.datos(i)(j)
        }
      }

      return nuevaMatriz
    }

    // Método para multiplicar dos matrices
    def *(other: Matriz): Matriz = {
      if (columnas != other.filas) {
        throw new IllegalArgumentException("Las matrices no se pueden multiplicar")
      }

      val nuevaMatriz = new Matriz(filas, other.columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until other.columnas) {
          for (k <- 0 until columnas) {
            nuevaMatriz.datos(i)(j) += datos(i)(k) * other.datos(k)(j)
          }
        }
      }

      return nuevaMatriz
    }

    // Método para calcular la inversa de una matriz
    def inversa: Matriz = {
      if (filas != columnas) {
        throw new IllegalArgumentException("La matriz no es cuadrada")
      }

      val identidad = new Matriz(filas, columnas)
      for (i <- 0 until filas) {
        identidad.datos(i)(i) = 1.0
      }

      val ampliada = new Matriz(filas, 2 * columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          ampliada.datos(i)(j) = datos(i)(j)
          ampliada.datos(i)(j + columnas) = identidad.datos(i)(j)
        }
      }

      // Realizar operaciones de fila para reducir la matriz ampliada a una forma triangular superior
      for (i <- 0 until filas) {
        for (j <- i + 1 until filas) {
          val factor = ampliada.datos(j)(i) / ampliada.datos(i)(i)
          for (k <- 0 until 2 * columnas) {
            ampliada.datos(j)(k) -= factor * ampliada.datos(i)(k)
          }
        }
      }

      // Realizar operaciones de fila para reducir la matriz triangular superior a una matriz identidad
      for (i <- filas - 1 to 0 by -1) {
        for (j <- i - 1 downTo 0) {
          val factor = ampliada.datos(j)(i) / ampliada.datos(i)(i)
          for (k <- 0 until 2 * columnas) {
            ampliada.datos(j)(k) -= factor * ampliada.datos(i)(k)
          }
        }
      }

      // Extraer la matriz identidad de la matriz ampliada
      val inversa = new Matriz(filas, columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          inversa.datos(i)(j) = ampliada.datos(i)(j + columnas)
        }
      }

      return inversa
    }

    // Método para calcular el determinante de una matriz
    def determinante: Double = {
      if (filas != columnas) {
        throw new IllegalArgumentException("La matriz no es cuadrada")
      }

      // Si la matriz es de tamaño 2, devolver el determinante directamente
      if (filas == 2) {
        return datos(0)(0) * datos(1)(1) - datos(0)(1) * datos(1)(0)
      }

      // Crear una matriz de cofactores
      val cofactores = new Matriz(filas, columnas)
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          val submatriz = obtenerSubmatriz(i, j)
          cofactores.datos(i)(j) = Math.pow(-1, i + j) * submatriz.determinante
        }
      }

      // Calcular el determinante como la suma de los elementos de la primera fila multiplicados por sus respectivos cofactores
      var determinante = 0.0
      for (j <- 0 until columnas) {
        determinante += datos(0)(j) * cofactores.datos(0)(j)
      }

      return determinante
    }

    // Método para obtener una submatriz de la matriz actual
    private def obtenerSubmatriz(fila: Int, columna: Int): Matriz = {
      val nuevaMatriz = new Matriz(filas - 1, columnas - 1)
      var iNuevo = 0
      for (i <- 0 until filas) {
        if (i != fila) {
          var jNuevo = 0
          for (j <- 0 until columnas) {
            if (j != columna) {
              nuevaMatriz.datos(iNuevo)(jNuevo) = datos(i)(j)
              jNuevo += 1
            }
          }
          iNuevo += 1
        }
      }

      return nuevaMatriz
    }

    // Método para imprimir la matriz en la consola
    def imprimir: Unit = {
      for (i <- 0 until filas) {
        for (j <- 0 until columnas) {
          print(f"${datos(i)(j)}%6.2f ")
        }
        println()
      }
    }

  }

  // Función principal del programa
  def main(args: Array[String]): Unit = {

    // Crear una matriz A
    val a = new Matriz(3, 3, Array(
      Array(1.0, 2.0, 3.0),
      Array(4.0, 5.0, 6.0),
      Array(7.0, 8.0, 9.0)
    ))

    // Crear una matriz B
    val b = new Matriz(3, 3, Array(
      Array(1.0, 0.0, 0.0),
      Array(0.0, 1.0, 0.0),
      Array(0.0, 0.0, 1.0)
    ))

    // Sumar las matrices A y B
    val c = a + b

    // Imprimir la matriz C
    c.imprimir

    println()

    // Restar las matrices A y B
    val d = a - b

    // Imprimir la matriz D
    d.imprimir

    println()

    // Multiplicar las matrices A y B
    val e = a * b

    // Imprimir la matriz E
    e.imprimir

    println()

    // Calcular la inversa de la matriz A
    val aInversa = a.inversa

    // Imprimir la matriz A inversa
    aInversa.imprimir

    println()

    // Calcular el determinante de la matriz A
    val aDeterminante = a.determinante

    // Imprimir el determinante de la matriz A
    println(f"El determinante de la matriz A es $aDeterminante%6.2f")

  }

}
```

Explicación del código:

* La clase `Matriz` representa una matriz numérica.
* El constructor de la matriz permite crear una matriz de un tamaño específico o una matriz vacía que luego se puede rellenar.
* Los métodos `+`, `-` y `*` permiten sumar, restar y multiplicar matrices, respectivamente.
* El método `inversa` calcula la matriz inversa de una matriz cuadrada.
* El método `determinante` calcula el determinante de una matriz cuadrada.
* El método `obtenerSubmatriz` obtiene una submatriz de una matriz dada, excluyendo una fila y una columna especificadas.
* El método `imprimir` imprime la matriz en la consola.
* La función `main` del programa crea dos matrices A y B, realiza operaciones con ellas y luego imprime los resultados.

Este código es un ejemplo de cómo se pueden usar matrices en Scala para realizar cálculos matemáticos complejos.