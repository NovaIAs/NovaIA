```scala
// Importamos las librerías necesarias
import scala.io.Source
import scala.util.control.Breaks._

// Creamos una función para leer un archivo de texto y devolver su contenido como una lista de líneas
def leerArchivo(rutaArchivo: String): List[String] = {
  // Intentamos abrir el archivo
  val archivo = try {
    Source.fromFile(rutaArchivo)
  } catch {
    case e: Exception =>
      println("Error al abrir el archivo: " + e.getMessage)
      return List()
  }

  // Leemos las líneas del archivo y las guardamos en una lista
  val lineas = archivo.getLines().toList

  // Cerramos el archivo
  archivo.close()

  // Devolvemos la lista de líneas
  lineas
}

// Creamos una función para contar el número de palabras en una lista de líneas
def contarPalabras(lineas: List[String]): Int = {
  // Creamos una lista vacía para almacenar las palabras
  var palabras = List[String]()

  // Iteramos sobre las líneas
  for (linea <- lineas) {
    // Dividimos la línea en palabras
    val palabrasLinea = linea.split(" ")

    // Añadimos las palabras a la lista
    palabras ++= palabrasLinea
  }

  // Devolvemos el número de palabras
  palabras.length
}

// Creamos una función para contar el número de líneas en blanco en una lista de líneas
def contarLineasEnBlanco(lineas: List[String]): Int = {
  // Creamos una lista vacía para almacenar las líneas en blanco
  var lineasEnBlanco = List[String]()

  // Iteramos sobre las líneas
  for (linea <- lineas) {
    // Si la línea está vacía, la añadimos a la lista
    if (linea.isEmpty) {
      lineasEnBlanco ++= List(linea)
    }
  }

  // Devolvemos el número de líneas en blanco
  lineasEnBlanco.length
}

// Creamos una función para contar el número de caracteres en una lista de líneas
def contarCaracteres(lineas: List[String]): Int = {
  // Creamos una variable para almacenar el número de caracteres
  var caracteres = 0

  // Iteramos sobre las líneas
  for (linea <- lineas) {
    // Añadimos el número de caracteres de la línea al total
    caracteres += linea.length
  }

  // Devolvemos el número de caracteres
  caracteres
}

// Pedimos al usuario que introduzca la ruta del archivo
println("Introduce la ruta del archivo:")
val rutaArchivo = scala.io.StdIn.readLine()

// Leemos el archivo
val lineas = leerArchivo(rutaArchivo)

// Si el archivo no se pudo leer, salimos del programa
if (lineas.isEmpty) {
  println("Error al leer el archivo.")
  sys.exit(1)
}

// Contamos el número de palabras, líneas en blanco y caracteres en el archivo
val palabras = contarPalabras(lineas)
val lineasEnBlanco = contarLineasEnBlanco(lineas)
val caracteres = contarCaracteres(lineas)

// Mostramos los resultados
println("Número de palabras: " + palabras)
println("Número de líneas en blanco: " + lineasEnBlanco)
println("Número de caracteres: " + caracteres)
```

Explicación del código:

* La función `leerArchivo()` lee un archivo de texto y devuelve su contenido como una lista de líneas.
* La función `contarPalabras()` cuenta el número de palabras en una lista de líneas.
* La función `contarLineasEnBlanco()` cuenta el número de líneas en blanco en una lista de líneas.
* La función `contarCaracteres()` cuenta el número de caracteres en una lista de líneas.
* En el bloque `main()` del programa, pedimos al usuario que introduzca la ruta del archivo que desea analizar.
* Leemos el archivo utilizando la función `leerArchivo()`.
* Si el archivo no se pudo leer, salimos del programa.
* Contamos el número de palabras, líneas en blanco y caracteres en el archivo utilizando las funciones `contarPalabras()`, `contarLineasEnBlanco()` y `contarCaracteres()`, respectivamente.
* Mostramos los resultados.