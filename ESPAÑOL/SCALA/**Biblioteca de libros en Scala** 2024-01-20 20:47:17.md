```scala
// Definir una clase que represente un libro
class Libro(titulo: String, autor: String, paginas: Int) {
  // Propiedades del libro
  var titulo: String = titulo
  var autor: String = autor
  var paginas: Int = paginas

  // Método para imprimir los detalles del libro
  def imprimirDetalles(): Unit = {
    println(s"Título: $titulo")
    println(s"Autor: $autor")
    println(s"Páginas: $paginas")
  }
}

// Definir una clase que represente una biblioteca
class Biblioteca {
  // Lista de libros en la biblioteca
  private var libros: List[Libro] = List()

  // Método para agregar un libro a la biblioteca
  def agregarLibro(libro: Libro): Unit = {
    libros = libros :+ libro
  }

  // Método para buscar un libro por su título
  def buscarLibroPorTitulo(titulo: String): Option[Libro] = {
    libros.find(_.titulo == titulo)
  }

  // Método para imprimir la lista de libros en la biblioteca
  def imprimirCatalogo(): Unit = {
    for (libro <- libros) {
      libro.imprimirDetalles()
      println()
    }
  }
}

// Crear una instancia de la biblioteca
val biblioteca = new Biblioteca()

// Agregar algunos libros a la biblioteca
biblioteca.agregarLibro(new Libro("El Quijote", "Miguel de Cervantes", 1023))
biblioteca.agregarLibro(new Libro("Cien años de soledad", "Gabriel García Márquez", 417))
biblioteca.agregarLibro(new Libro("El Principito", "Antoine de Saint-Exupéry", 91))

// Buscar un libro por su título
val libroBuscado = biblioteca.buscarLibroPorTitulo("El Quijote")

// Imprimir la información del libro buscado
if (libroBuscado.isDefined) {
  println("El libro buscado es:")
  libroBuscado.get.imprimirDetalles()
} else {
  println("El libro buscado no se encuentra en la biblioteca.")
}

// Imprimir el catálogo de la biblioteca
println("Catálogo de la biblioteca:")
biblioteca.imprimirCatalogo()
```

Este código define una clase `Libro` que representa un libro con propiedades como título, autor y número de páginas. También define una clase `Biblioteca` que mantiene una lista de libros y proporciona métodos para agregar, buscar e imprimir libros.

Para utilizar estas clases, primero se crea una instancia de la biblioteca y se agregan algunos libros a ella. Luego, se busca un libro por su título y se imprime su información. Finalmente, se imprime el catálogo de la biblioteca, que es la lista de todos los libros en la biblioteca.

Este código es complejo porque utiliza varias características del lenguaje Scala, como clases, métodos, listas y opciones. También utiliza la interpolación de cadenas para imprimir información de manera más concisa.