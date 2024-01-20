```scala
// Clase `Persona` que representa una persona con nombre y edad
class Persona(nombre: String, edad: Int) {
  // Atributos
  var nombre: String = nombre
  var edad: Int = edad

  // Métodos
  def saludar(): Unit = {
    println(s"Hola, mi nombre es $nombre y tengo $edad años.")
  }
}

// Clase `Estudiante` que hereda de `Persona` y añade el atributo `curso`
class Estudiante(nombre: String, edad: Int, curso: String) extends Persona(nombre, edad) {
  // Atributos
  var curso: String = curso

  // Métodos
  def estudiar(): Unit = {
    println(s"Estoy estudiando $curso.")
  }
}

// Clase `Profesor` que hereda de `Persona` y añade el atributo `asignatura`
class Profesor(nombre: String, edad: Int, asignatura: String) extends Persona(nombre, edad) {
  // Atributos
  var asignatura: String = asignatura

  // Métodos
  def enseñar(): Unit = {
    println(s"Estoy enseñando $asignatura.")
  }
}

// Función `main` que crea un objeto `Estudiante` y un objeto `Profesor` y los llama a sus métodos
object Main {
  def main(args: Array[String]): Unit = {
    val estudiante = new Estudiante("Juan", 20, "Informática")
    estudiante.saludar()
    estudiante.estudiar()

    val profesor = new Profesor("María", 40, "Matemáticas")
    profesor.saludar()
    profesor.enseñar()
  }
}
```

Explicación:

* Las clases `Persona`, `Estudiante` y `Profesor` representan diferentes tipos de personas con diferentes atributos y métodos.
* La clase `Persona` es la superclase de `Estudiante` y `Profesor`, y define los atributos y métodos comunes a todas las personas.
* La clase `Estudiante` hereda de `Persona` y añade el atributo `curso` y el método `estudiar()`.
* La clase `Profesor` hereda de `Persona` y añade el atributo `asignatura` y el método `enseñar()`.
* La función `main` crea un objeto `Estudiante` y un objeto `Profesor`, y los llama a sus métodos.

Este código demuestra los conceptos básicos de la herencia en Scala, incluyendo la creación de clases, la herencia de atributos y métodos, y el uso de objetos.