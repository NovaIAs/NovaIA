```scala
// Definición de la clase Libro
class Libro(titulo: String, autor: String, ISBN: String) {
  // Atributos de la clase Libro
  var titulo: String = titulo
  var autor: String = autor
  var ISBN: String = ISBN

  // Método para obtener el título del libro
  def getTitulo(): String = {
    return titulo
  }

  // Método para obtener el autor del libro
  def getAutor(): String = {
    return autor
  }

  // Método para obtener el ISBN del libro
  def getISBN(): String = {
    return ISBN
  }

  // Método para imprimir la información del libro
  def imprimirInfo(): Unit = {
    println("Título: " + titulo)
    println("Autor: " + autor)
    println("ISBN: " + ISBN)
  }
}

// Definición de la clase Biblioteca
class Biblioteca() {
  // Atributos de la clase Biblioteca
  var libros: List[Libro] = List()

  // Método para agregar un libro a la biblioteca
  def agregarLibro(libro: Libro): Unit = {
    libros = libro :: libros
  }

  // Método para obtener el libro con mayor ISBN
  def getLibroMayorISBN(): Libro = {
    var libroMayorISBN: Libro = libros.head
    for (libro <- libros) {
      if (libro.getISBN() > libroMayorISBN.getISBN()) {
        libroMayorISBN = libro
      }
    }
    return libroMayorISBN
  }

  // Método para obtener el libro con menor ISBN
  def getLibroMenorISBN(): Libro = {
    var libroMenorISBN: Libro = libros.head
    for (libro <- libros) {
      if (libro.getISBN() < libroMenorISBN.getISBN()) {
        libroMenorISBN = libro
      }
    }
    return libroMenorISBN
  }

  // Método para imprimir la información de todos los libros de la biblioteca
  def imprimirInfoLibros(): Unit = {
    for (libro <- libros) {
      libro.imprimirInfo()
      println()
    }
  }
}

// Función principal
object Main {
  def main(args: Array[String]): Unit = {
    // Creación de una biblioteca
    val biblioteca = new Biblioteca()

    // Creación de libros
    val libro1 = new Libro("El Quijote", "Miguel de Cervantes", "978-84-9830-768-3")
    val libro2 = new Libro("La Divina Comedia", "Dante Alighieri", "978-84-9830-769-0")
    val libro3 = new Libro("El Señor de los Anillos", "J. R. R. Tolkien", "978-84-9830-770-6")

    // Añadir los libros a la biblioteca
    biblioteca.agregarLibro(libro1)
    biblioteca.agregarLibro(libro2)
    biblioteca.agregarLibro(libro3)

    // Obtener el libro con mayor ISBN
    val libroMayorISBN = biblioteca.getLibroMayorISBN()

    // Obtener el libro con menor ISBN
    val libroMenorISBN = biblioteca.getLibroMenorISBN()

    // Imprimir la información de todos los libros de la biblioteca
    println("Libros en la biblioteca:")
    biblioteca.imprimirInfoLibros()

    // Imprimir el libro con mayor ISBN
    println("Libro con mayor ISBN:")
    libroMayorISBN.imprimirInfo()

    // Imprimir el libro con menor ISBN
    println("Libro con menor ISBN:")
    libroMenorISBN.imprimirInfo()
  }
}
```

Explicación del código:

* La clase `Libro` representa un libro con atributos como `titulo`, `autor` e `ISBN`.
* La clase `Biblioteca` representa una biblioteca que contiene una lista de libros.
* La función `main` crea una biblioteca, agrega libros a la biblioteca, obtiene el libro con mayor ISBN, obtiene el libro con menor ISBN e imprime la información de todos los libros de la biblioteca.
* El código utiliza conceptos de programación orientada a objetos, listas, iteración y funciones.