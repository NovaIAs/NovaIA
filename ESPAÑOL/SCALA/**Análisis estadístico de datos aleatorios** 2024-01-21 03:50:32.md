```scala
import scala.collection.mutable.ListBuffer
import scala.util.Random

object CodigoComplejo {

  def main(args: Array[String]): Unit = {
    // Creamos una lista de números aleatorios
    val numeros = new ListBuffer[Int]
    for (i <- 1 to 100) {
      numeros += Random.nextInt(100)
    }

    // Ordenamos la lista de números
    val numerosOrdenados = numeros.sorted

    // Creamos un mapa con los números como claves y su frecuencia como valores
    val mapaNumeros = new scala.collection.mutable.HashMap[Int, Int]
    for (numero <- numerosOrdenados) {
      if (mapaNumeros.contains(numero)) {
        mapaNumeros(numero) += 1
      } else {
        mapaNumeros(numero) = 1
      }
    }

    // Imprimimos el mapa con los números y su frecuencia
    for ((numero, frecuencia) <- mapaNumeros) {
      println(s"$numero: $frecuencia")
    }

    // Calculamos la media y la desviación estándar de los números
    val media = numerosOrdenados.sum / numerosOrdenados.length
    val desviacionEstandar = math.sqrt(numerosOrdenados.map(numero => math.pow(numero - media, 2)).sum / numerosOrdenados.length)

    // Imprimimos la media y la desviación estándar
    println(s"Media: $media")
    println(s"Desviación estándar: $desviacionEstandar")

    // Creamos una lista de listas de números aleatorios
    val listasNumeros = new ListBuffer[List[Int]]
    for (i <- 1 to 10) {
      val listaNumeros = new ListBuffer[Int]
      for (j <- 1 to 100) {
        listaNumeros += Random.nextInt(100)
      }
      listasNumeros += listaNumeros.toList
    }

    // Calculamos la media de cada lista de números
    val mediasListas = listasNumeros.map(listaNumeros => listaNumeros.sum / listaNumeros.length)

    // Imprimimos la media de cada lista de números
    for (mediaLista <- mediasListas) {
      println(s"Media lista: $mediaLista")
    }

    // Creamos una matriz de números aleatorios
    val matrizNumeros = Array.ofDim[Int](10, 10)
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        matrizNumeros(i)(j) = Random.nextInt(100)
      }
    }

    // Imprimimos la matriz de números
    for (i <- 0 until 10) {
      for (j <- 0 until 10) {
        print(s"${matrizNumeros(i)(j)} ")
      }
      println()
    }

    // Calculamos la suma de cada fila de la matriz
    val sumasFilas = matrizNumeros.map(fila => fila.sum)

    // Imprimimos la suma de cada fila de la matriz
    for (sumaFila <- sumasFilas) {
      println(s"Suma fila: $sumaFila")
    }

    // Calculamos la suma de cada columna de la matriz
    val sumasColumnas = matrizNumeros.transpose.map(columna => columna.sum)

    // Imprimimos la suma de cada columna de la matriz
    for (sumaColumna <- sumasColumnas) {
      println(s"Suma columna: $sumaColumna")
    }
  }
}
```

Explicación del código:

1. Creamos una lista de números aleatorios y la ordenamos.

2. Creamos un mapa con los números como claves y su frecuencia como valores.

3. Imprimimos el mapa con los números y su frecuencia.

4. Calculamos la media y la desviación estándar de los números y los imprimimos.

5. Creamos una lista de listas de números aleatorios y calculamos la media de cada lista.

6. Imprimimos la media de cada lista de números.

7. Creamos una matriz de números aleatorios y la imprimimos.

8. Calculamos la suma de cada fila y cada columna de la matriz y las imprimimos.