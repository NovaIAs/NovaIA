```scala
// Declarar una clase de objetos Libro que contenga información sobre un libro, como su título, autor y año de publicación.

class Libro(val título: String, val autor: String, val añoPublicación: Int) {

  override def toString = s"Libro: $título, Autor: $autor, Año de publicación: $añoPublicación"

}

// Crear una función principal para ejecutar el programa.

object Main {

  def main(args: Array[String]): Unit = {

    // Crear una lista de libros.

    val libros = List(
      new Libro("El ingenioso hidalgo don Quijote de la Mancha", "Miguel de Cervantes Saavedra", 1605),
      new Libro("Cien años de soledad", "Gabriel García Márquez", 1967),
      new Libro("El principito", "Antoine de Saint-Exupéry", 1943)
    )

    // Imprimir la lista de libros usando un bucle for.

    println("Lista de libros:")
    for (libro <- libros) {
      println(libro)
    }

    // Utilizar la función de ordenación sort para ordenar la lista de libros por su título.

    val librosOrdenadosPorTítulo = libros.sortWith((a, b) => a.título.compareTo(b.título) < 0)

    // Imprimir la lista de libros ordenados por título.

    println("Lista de libros ordenados por título:")
    for (libro <- librosOrdenadosPorTítulo) {
      println(libro)
    }

    // Utilizar la función de filtrado filter para obtener una lista de libros publicados después de 1950.

    val librosPublicadosDespuésDe1950 = libros.filter(_.añoPublicación > 1950)

    // Imprimir la lista de libros publicados después de 1950.

    println("Lista de libros publicados después de 1950:")
    for (libro <- librosPublicadosDespuésDe1950) {
      println(libro)
    }

    // Utilizar la función de map para obtener una lista de los títulos de los libros.

    val títulosLibros = libros.map(_.título)

    // Imprimir la lista de títulos de los libros.

    println("Lista de títulos de los libros:")
    for (título <- títulosLibros) {
      println(título)
    }

  }

}
```

Explicación del código:

* La clase `Libro` representa un libro con su título, autor y año de publicación.
* La función `main` es la función principal del programa y contiene el código a ejecutar.
* La lista `libros` contiene una lista de objetos `Libro`.
* El bucle `for` imprime la lista de libros.
* La función `sort` ordena la lista de libros por su título.
* La función `filter` filtra la lista de libros para obtener los libros publicados después de 1950.
* La función `map` obtiene una lista de los títulos de los libros.
* El bucle `for` imprime la lista de títulos de los libros.