```scala
// Importamos las librerías necesarias.
import scala.collection.mutable.ListBuffer
import scala.io.Source

// Definimos una clase "Archivo" que representará cada archivo de texto.
class Archivo(val nombre: String, val contenido: String)

// Definimos una función que leerá el contenido de un archivo y lo devolverá como una cadena de texto.
def leerArchivo(nombreArchivo: String): String = {
  val contenido = Source.fromFile(nombreArchivo).mkString
  contenido
}

// Definimos una función que buscará una palabra dentro del contenido de un archivo y devolverá una lista con las posiciones en las que se encuentra.
def buscarPalabra(palabra: String, archivo: Archivo): List[Int] = {
  var posiciones = ListBuffer[Int]()
  var index = archivo.contenido.indexOf(palabra)
  while (index != -1) {
    posiciones += index
    index = archivo.contenido.indexOf(palabra, index + 1)
  }
  posiciones.toList
}

// Definimos una función que imprimirá los resultados de la búsqueda de una palabra en un archivo.
def imprimirResultados(palabra: String, archivo: Archivo, posiciones: List[Int]): Unit = {
  println(s"Palabra: $palabra")
  println(s"Archivo: ${archivo.nombre}")
  println("Posiciones:")
  posiciones.foreach(posicion => println(posicion))
}

// Definimos una función que buscará una palabra en todos los archivos de una lista y devolverá una lista con los resultados.
def buscarPalabraEnArchivos(palabra: String, archivos: List[Archivo]): List[(Archivo, List[Int])] = {
  var resultados = ListBuffer[(Archivo, List[Int])]()
  archivos.foreach(archivo => {
    val posiciones = buscarPalabra(palabra, archivo)
    if (posiciones.nonEmpty) {
      resultados += ((archivo, posiciones))
    }
  })
  resultados.toList
}

// Definimos una función que imprimirá los resultados de la búsqueda de una palabra en todos los archivos de una lista.
def imprimirResultadosEnArchivos(palabra: String, resultados: List[(Archivo, List[Int])]): Unit = {
  println(s"Palabra: $palabra")
  println("Resultados:")
  resultados.foreach(resultado => {
    val (archivo, posiciones) = resultado
    imprimirResultados(palabra, archivo, posiciones)
  })
}

// Definimos una función que ejecutará el programa.
def main(args: Array[String]): Unit = {
  // Leemos los archivos de texto de la carpeta "archivos".
  val archivos = ListBuffer[Archivo]()
  val rutaArchivos = "archivos"
  val ficheros = new java.io.File(rutaArchivos).listFiles()
  ficheros.foreach(fichero => {
    if (fichero.isFile) {
      val nombreArchivo = fichero.getName
      val contenidoArchivo = leerArchivo(nombreArchivo)
      val archivo = new Archivo(nombreArchivo, contenidoArchivo)
      archivos += archivo
    }
  })

  // Pedimos al usuario que introduzca una palabra a buscar.
  print("Introduce una palabra a buscar: ")
  val palabra = scala.io.StdIn.readLine()

  // Buscamos la palabra en todos los archivos.
  val resultados = buscarPalabraEnArchivos(palabra, archivos.toList)

  // Imprimimos los resultados de la búsqueda.
  imprimirResultadosEnArchivos(palabra, resultados)
}

// Ejecutamos el programa.
main(args)
```

Explicación del código:

* La clase "Archivo" representa cada archivo de texto y tiene dos propiedades: el nombre del archivo y el contenido del archivo.
* La función "leerArchivo" lee el contenido de un archivo y lo devuelve como una cadena de texto.
* La función "buscarPalabra" busca una palabra dentro del contenido de un archivo y devuelve una lista con las posiciones en las que se encuentra.
* La función "imprimirResultados" imprime los resultados de la búsqueda de una palabra en un archivo.
* La función "buscarPalabraEnArchivos" busca una palabra en todos los archivos de una lista y devuelve una lista con los resultados.
* La función "imprimirResultadosEnArchivos" imprime los resultados de la búsqueda de una palabra en todos los archivos de una lista.
* La función "main" ejecuta el programa y pide al usuario que introduzca una palabra a buscar.
* El programa lee los archivos de texto de la carpeta "archivos", busca la palabra introducida por el usuario en todos los archivos y imprime los resultados de la búsqueda.