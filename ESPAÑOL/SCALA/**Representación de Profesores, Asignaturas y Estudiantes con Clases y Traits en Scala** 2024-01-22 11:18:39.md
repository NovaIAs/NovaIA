```scala
// Definición de clases y traits

// Clase Profesor
class Profesor(val nombre: String, val apellidos: String, val edad: Int) {

  // Método para obtener el nombre completo del profesor
  def obtenerNombreCompleto: String = {
    s"$nombre $apellidos"
  }

  // Método para obtener la edad del profesor
  def obtenerEdad: Int = {
    edad
  }

  // Método para imprimir la información del profesor
  def imprimirInformacion: Unit = {
    println(s"Nombre: $nombre")
    println(s"Apellidos: $apellidos")
    println(s"Edad: $edad")
  }
}

// Trait Asignatura
trait Asignatura {

  // Método para obtener el nombre de la asignatura
  def obtenerNombre: String

  // Método para obtener la carga horaria de la asignatura
  def obtenerCargaHoraria: Int

  // Método para imprimir la información de la asignatura
  def imprimirInformacion: Unit
}

// Clase Matemáticas
class Matemáticas extends Asignatura {

  // Nombre de la asignatura
  val nombre = "Matemáticas"

  // Carga horaria de la asignatura
  val cargaHoraria = 4

  // Método para obtener el nombre de la asignatura
  override def obtenerNombre: String = {
    nombre
  }

  // Método para obtener la carga horaria de la asignatura
  override def obtenerCargaHoraria: Int = {
    cargaHoraria
  }

  // Método para imprimir la información de la asignatura
  override def imprimirInformacion: Unit = {
    println(s"Nombre: $nombre")
    println(s"Carga horaria: $cargaHoraria")
  }
}

// Clase Física
class Física extends Asignatura {

  // Nombre de la asignatura
  val nombre = "Física"

  // Carga horaria de la asignatura
  val cargaHoraria = 3

  // Método para obtener el nombre de la asignatura
  override def obtenerNombre: String = {
    nombre
  }

  // Método para obtener la carga horaria de la asignatura
  override def obtenerCargaHoraria: Int = {
    cargaHoraria
  }

  // Método para imprimir la información de la asignatura
  override def imprimirInformacion: Unit = {
    println(s"Nombre: $nombre")
    println(s"Carga horaria: $cargaHoraria")
  }
}

// Clase Química
class Química extends Asignatura {

  // Nombre de la asignatura
  val nombre = "Química"

  // Carga horaria de la asignatura
  val cargaHoraria = 2

  // Método para obtener el nombre de la asignatura
  override def obtenerNombre: String = {
    nombre
  }

  // Método para obtener la carga horaria de la asignatura
  override def obtenerCargaHoraria: Int = {
    cargaHoraria
  }

  // Método para imprimir la información de la asignatura
  override def imprimirInformacion: Unit = {
    println(s"Nombre: $nombre")
    println(s"Carga horaria: $cargaHoraria")
  }
}

// Clase Estudiante
class Estudiante(val nombre: String, val apellidos: String, val edad: Int) {

  // Lista de asignaturas que cursa el estudiante
  var asignaturas: List[Asignatura] = List()

  // Método para añadir una asignatura a la lista de asignaturas
  def añadirAsignatura(asignatura: Asignatura): Unit = {
    asignaturas = asignaturas :+ asignatura
  }

  // Método para obtener la lista de asignaturas que cursa el estudiante
  def obtenerAsignaturas: List[Asignatura] = {
    asignaturas
  }

  // Método para imprimir la información del estudiante
  def imprimirInformacion: Unit = {
    println(s"Nombre: $nombre")
    println(s"Apellidos: $apellidos")
    println(s"Edad: $edad")
    println(s"Asignaturas:")
    for (asignatura <- asignaturas) {
      asignatura.imprimirInformacion
    }
  }
}

// Creación de objetos

// Objetos de la clase Profesor
val profesor1 = new Profesor("Juan", "García", 40)
val profesor2 = new Profesor("María", "López", 35)

// Objetos de la clase Matemáticas
val matemáticas1 = new Matemáticas

// Objetos de la clase Física
val física1 = new Física

// Objetos de la clase Química
val química1 = new Química

// Objetos de la clase Estudiante
val estudiante1 = new Estudiante("Pedro", "Sánchez", 20)
val estudiante2 = new Estudiante("Ana", "Fernández", 21)

// Asignación de asignaturas a los estudiantes
estudiante1.añadirAsignatura(matemáticas1)
estudiante1.añadirAsignatura(física1)
estudiante2.añadirAsignatura(química1)

// Impresión de la información de los profesores y estudiantes
profesor1.imprimirInformacion
profesor2.imprimirInformacion
estudiante1.imprimirInformacion
estudiante2.imprimirInformacion
```

Explicación del código:

* Se definen las clases `Profesor`, `Asignatura`, `Matemáticas`, `Física`, `Química` y `Estudiante` con sus respectivas propiedades y métodos.
* Se crean instancias de las clases anteriores para representar a los profesores, las asignaturas y los estudiantes.
* Se añaden las asignaturas a los estudiantes.
* Finalmente, se imprime la información de los profesores y los estudiantes.

Este código es complejo porque:

* Utiliza clases y traits para representar diferentes conceptos.
* Implementa métodos para obtener y modificar los datos de los objetos.
* Utiliza listas para almacenar las asignaturas que cursan los estudiantes.
* Imprime la información de los objetos de forma ordenada.

Este código es útil para representar y gestionar la información de profesores, asignaturas y estudiantes en un sistema educativo.