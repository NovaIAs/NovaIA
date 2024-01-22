```scala
// Importamos las librerías necesarias
import scala.io.Source
import scala.util.Using

// Definimos la función para obtener el texto del archivo
def obtenerTextoArchivo(rutaArchivo: String): String = {
  var textoArchivo = ""
  Using(Source.fromFile(rutaArchivo)) { source =>
    textoArchivo = source.mkString
  }
  textoArchivo
}

// Definimos la función para contar las palabras del texto
def contarPalabras(texto: String): Int = {
  texto.split("\\s+").length
}

// Definimos la función para obtener la frecuencia de las palabras del texto
def obtenerFrecuenciaPalabras(texto: String): Map[String, Int] = {
  val palabras = texto.split("\\s+")
  palabras.groupBy(palabra => palabra).mapValues(_.length)
}

// Definimos la función para obtener las palabras más frecuentes del texto
def obtenerPalabrasMasFrecuentes(texto: String, limitePalabras: Int): List[(String, Int)] = {
  val frecuenciaPalabras = obtenerFrecuenciaPalabras(texto)
  frecuenciaPalabras.toList.sortBy(_._2)(Ordering.Int.reverse).take(limitePalabras)
}

// Definimos la función para imprimir los resultados
def imprimirResultados(palabrasMasFrecuentes: List[(String, Int)]): Unit = {
  println("Palabras más frecuentes:")
  palabrasMasFrecuentes.foreach { palabra =>
    println(s"${palabra._1}: ${palabra._2}")
  }
}

// Obtenemos el texto del archivo
val rutaArchivo = "archivo.txt"
val textoArchivo = obtenerTextoArchivo(rutaArchivo)

// Contamos las palabras del texto
val numeroPalabras = contarPalabras(textoArchivo)
println(s"Número de palabras en el archivo: $numeroPalabras")

// Obtenemos la frecuencia de las palabras del texto
val frecuenciaPalabras = obtenerFrecuenciaPalabras(textoArchivo)
println("Frecuencia de las palabras en el archivo:")
frecuenciaPalabras.foreach { palabra =>
  println(s"${palabra._1}: ${palabra._2}")
}

// Obtenemos las palabras más frecuentes del texto
val palabrasMasFrecuentes = obtenerPalabrasMasFrecuentes(textoArchivo, 10)
imprimirResultados(palabrasMasFrecuentes)
```

Explicación del código:

1. Importamos las librerías necesarias, incluyendo `Source` y `Using` para leer archivos y gestionar recursos.


2. Definimos la función `obtenerTextoArchivo` para obtener el texto del archivo especificado.


3. Definimos la función `contarPalabras` para contar el número de palabras en un texto.


4. Definimos la función `obtenerFrecuenciaPalabras` para obtener la frecuencia de las palabras en un texto.


5. Definimos la función `obtenerPalabrasMasFrecuentes` para obtener las palabras más frecuentes en un texto y ordenarlas en orden descendente.


6. Definimos la función `imprimirResultados` para imprimir los resultados.


7. Obtenemos el texto del archivo usando la función `obtenerTextoArchivo`.


8. Contamos las palabras del texto usando la función `contarPalabras` e imprimimos el resultado.


9. Obtenemos la frecuencia de las palabras del texto usando la función `obtenerFrecuenciaPalabras` e imprimimos el resultado.


10. Obtenemos las palabras más frecuentes del texto usando la función `obtenerPalabrasMasFrecuentes` e imprimimos el resultado.