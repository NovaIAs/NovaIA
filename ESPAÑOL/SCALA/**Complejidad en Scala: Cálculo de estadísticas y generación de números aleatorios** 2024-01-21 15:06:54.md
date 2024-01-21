```scala
import scala.io.StdIn
import scala.util.Random

object ComplejidadScala {

  // Función principal del programa
  def main(args: Array[String]): Unit = {

    // Crea una lista vacía de enteros
    var listaEnteros = List[Int]()

    // Solicita al usuario que ingrese enteros, separados por espacios
    println("Ingrese una lista de enteros, separados por espacios:")
    val entrada = StdIn.readLine()

    // Convierte la entrada del usuario en una lista de enteros
    listaEnteros = entrada.split(" ").map(_.toInt).toList

    // Si el usuario no ingresó ningún entero, se lo indica y termina el programa
    if (listaEnteros.isEmpty) {
      println("No ingresó ningún entero. Fin del programa.")
      return
    }

    // Calcula la suma de los enteros en la lista
    val sumaEnteros = listaEnteros.sum

    // Calcula el promedio de los enteros en la lista
    val promedioEnteros = sumaEnteros / listaEnteros.length

    // Calcula el máximo y el mínimo de los enteros en la lista
    val (maxEntero, minEntero) = listaEnteros.foldLeft((Int.MinValue, Int.MaxValue)) {
      case ((max, min), entero) => (max.max(entero), min.min(entero))
    }

    // Genera un número aleatorio entre 1 y 100
    val numeroAleatorio = Random.nextInt(100) + 1

    // Imprime los resultados en la consola
    println(s"Suma de los enteros: $sumaEnteros")
    println(s"Promedio de los enteros: $promedioEnteros")
    println(s"Máximo entero: $maxEntero")
    println(s"Mínimo entero: $minEntero")
    println(s"Número aleatorio: $numeroAleatorio")

  }

}
```

Explicación del código:

* Se crea una lista vacía de enteros, `listaEnteros`.
* Se solicita al usuario que ingrese enteros, separados por espacios.
* Se convierte la entrada del usuario en una lista de enteros, utilizando la función `split()` para dividir la entrada en una lista de cadenas, y luego la función `map()` para convertir cada cadena en un entero.
* Si el usuario no ingresó ningún entero, se lo indica y termina el programa.
* Se calcula la suma de los enteros en la lista, utilizando la función `sum()`.
* Se calcula el promedio de los enteros en la lista, dividiendo la suma entre la longitud de la lista.
* Se calcula el máximo y el mínimo de los enteros en la lista, utilizando la función `foldLeft()` para recorrer la lista y actualizar los valores máximo y mínimo.
* Se genera un número aleatorio entre 1 y 100, utilizando la función `nextInt()` de la clase `Random`.
* Se imprimen los resultados en la consola.