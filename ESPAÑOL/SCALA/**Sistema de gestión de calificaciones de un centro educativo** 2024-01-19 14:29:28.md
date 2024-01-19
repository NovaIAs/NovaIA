```scala
// Importar las librerías necesarias
import scala.collection.mutable.ListBuffer
import scala.collection.immutable.HashMap
import scala.util.Random

// Crear una clase para representar a un estudiante
class Estudiante(val nombre: String, val edad: Int, val notas: List[Double]) {
  // Calcular la nota media del estudiante
  def notaMedia: Double = notas.sum / notas.length

  // Imprimir los datos del estudiante
  def imprimirDatos(): Unit = {
    println(s"Nombre: $nombre")
    println(s"Edad: $edad")
    println(s"Notas: ${notas.mkString(", ")}")
    println(s"Nota media: $notaMedia")
  }
}

// Crear una clase para representar a una asignatura
class Asignatura(val nombre: String, val profesor: String) {
  // Crear una lista de estudiantes que cursan la asignatura
  val estudiantes = new ListBuffer[Estudiante]

  // Añadir un estudiante a la lista de estudiantes
  def añadirEstudiante(estudiante: Estudiante): Unit = {
    estudiantes += estudiante
  }

  // Calcular la nota media de la asignatura
  def notaMedia: Double = {
    var sumaNotas = 0.0
    for (estudiante <- estudiantes) {
      sumaNotas += estudiante.notaMedia
    }
    sumaNotas / estudiantes.length
  }

  // Imprimir los datos de la asignatura
  def imprimirDatos(): Unit = {
    println(s"Nombre: $nombre")
    println(s"Profesor: $profesor")
    println("Estudiantes:")
    for (estudiante <- estudiantes) {
      estudiante.imprimirDatos()
    }
    println(s"Nota media: $notaMedia")
  }
}

// Crear una clase para representar a un centro educativo
class CentroEducativo(val nombre: String, val direccion: String) {
  // Crear una lista de asignaturas que se imparten en el centro
  val asignaturas = new ListBuffer[Asignatura]

  // Añadir una asignatura a la lista de asignaturas
  def añadirAsignatura(asignatura: Asignatura): Unit = {
    asignaturas += asignatura
  }

  // Calcular la nota media del centro educativo
  def notaMedia: Double = {
    var sumaNotas = 0.0
    for (asignatura <- asignaturas) {
      sumaNotas += asignatura.notaMedia
    }
    sumaNotas / asignaturas.length
  }

  // Imprimir los datos del centro educativo
  def imprimirDatos(): Unit = {
    println(s"Nombre: $nombre")
    println(s"Dirección: $direccion")
    println("Asignaturas:")
    for (asignatura <- asignaturas) {
      asignatura.imprimirDatos()
    }
    println(s"Nota media: $notaMedia")
  }
}

// Crear un centro educativo
val centroEducativo = new CentroEducativo("IES Pablo Neruda", "Calle Mayor, 123")

// Crear algunas asignaturas
val asignatura1 = new Asignatura("Matemáticas", "Juan García")
val asignatura2 = new Asignatura("Lengua Castellana", "María Pérez")
val asignatura3 = new Asignatura("Ciencias Naturales", "Pedro Sánchez")

// Añadir las asignaturas al centro educativo
centroEducativo.añadirAsignatura(asignatura1)
centroEducativo.añadirAsignatura(asignatura2)
centroEducativo.añadirAsignatura(asignatura3)

// Crear algunos estudiantes
val estudiante1 = new Estudiante("Juan López", 16, List(8.0, 7.0, 9.0))
val estudiante2 = new Estudiante("María García", 17, List(9.0, 8.0, 10.0))
val estudiante3 = new Estudiante("Pedro Sánchez", 18, List(7.0, 6.0, 8.0))

// Añadir los estudiantes a las asignaturas
asignatura1.añadirEstudiante(estudiante1)
asignatura1.añadirEstudiante(estudiante2)
asignatura2.añadirEstudiante(estudiante1)
asignatura2.añadirEstudiante(estudiante3)
asignatura3.añadirEstudiante(estudiante2)
asignatura3.añadirEstudiante(estudiante