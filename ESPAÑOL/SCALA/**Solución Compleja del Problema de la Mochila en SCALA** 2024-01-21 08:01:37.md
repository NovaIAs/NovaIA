El siguiente código es una solución compleja al problema de la Knapsack en SCALA:

```
object Knapsack {
  def main(args: Array[String]): Unit = {
    // Definición de las variables
    val objetos = Array(
      ("Objeto 1", 3, 4),
      ("Objeto 2", 4, 5),
      ("Objeto 3", 5, 6),
      ("Objeto 4", 6, 7)
    )
    val capacidad = 10

    // Creación de la tabla bidimensional para almacenar los resultados
    val tabla = Array.ofDim[Int](capacidad + 1, objetos.length + 1)

    // Llenado de la tabla
    for (i <- 1 to capacidad) {
      for (j <- 1 to objetos.length) {
        val objeto = objetos(j - 1)
        val peso = objeto._2
        val valor = objeto._3
        if (peso <= i) {
          tabla(i)(j) = max(tabla(i - peso)(j - 1) + valor, tabla(i)(j - 1))
        } else {
          tabla(i)(j) = tabla(i)(j - 1)
        }
      }
    }

    // Recuperación de la solución
    var pesoActual = capacidad
    var objetosElegidos = List[String]()
    for (j <- objetos.length to 1 by -1) {
      if (tabla(pesoActual)(j) != tabla(pesoActual)(j - 1)) {
        objetosElegidos ::= objetos(j - 1)._1
        pesoActual -= objetos(j - 1)._2
      }
    }

    // Impresión del resultado
    println("Objetos elegidos:")
    for (objeto <- objetosElegidos) {
      println(objeto)
    }
    println("Valor total: " + tabla(capacidad)(objetos.length))
  }

  def max(a: Int, b: Int): Int = {
    if (a > b) a else b
  }
}
```

Explicación del código:

1. Definición de las variables:
   * `objetos`: Array con las tuplas de objetos, donde cada tupla contiene el nombre del objeto, su peso y su valor.
   * `capacidad`: Capacidad máxima de la mochila.

2. Creación de la tabla bidimensional `tabla` para almacenar los resultados. La tabla tiene `capacidad + 1` filas y `objetos.length + 1` columnas.

3. Llenado de la tabla:
   * Se recorren las filas de la tabla (desde la segunda fila hasta la fila `capacidad + 1`) y las columnas de la tabla (desde la segunda columna hasta la columna `objetos.length + 1`).
   * Para cada celda de la tabla, se comprueba si el peso del objeto actual es menor o igual que el peso máximo permitido en la mochila para la fila actual. Si es así, se calcula el valor máximo entre dos opciones:
     * Incluir el objeto actual en la mochila y sumar su valor al valor máximo obtenido hasta el momento.
     * No incluir el objeto actual en la mochila y mantener el valor máximo obtenido hasta el momento.
   * Si el peso del objeto actual es mayor que el peso máximo permitido en la mochila para la fila actual, se deja el valor máximo obtenido hasta el momento.

4. Recuperación de la solución:
   * Se parte del peso máximo permitido en la mochila y se recorren las columnas de la tabla desde la última columna hasta la primera columna.
   * Para cada columna, se comprueba si el valor máximo obtenido hasta el momento es igual al valor máximo obtenido hasta el momento sin incluir el objeto actual. Si no son iguales, significa que el objeto actual ha sido incluido en la mochila, por lo que se añade su nombre a la lista de objetos elegidos y se reduce el peso máximo permitido en la mochila por el peso del objeto actual.

5. Impresión del resultado:
   * Se imprimen los objetos elegidos y el valor total obtenido.