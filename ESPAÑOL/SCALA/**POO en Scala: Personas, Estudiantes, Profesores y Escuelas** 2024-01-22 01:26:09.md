```scala
// Definición de una clase que representa una persona.
class Persona(nombre: String, edad: Int) {

  // Atributos de la clase Persona.
  val nombre: String = nombre
  val edad: Int = edad

  // Métodos de la clase Persona.
  def saludar(): String = {
    "Hola, mi nombre es " + nombre + " y tengo " + edad + " años."
  }
}

// Definición de una clase que representa un estudiante.
class Estudiante(nombre: String, edad: Int, carrera: String) extends Persona(nombre, edad) {

  // Atributos de la clase Estudiante.
  val carrera: String = carrera

  // Métodos de la clase Estudiante.
  def estudiar(): String = {
    "Estoy estudiando " + carrera + "."
  }
}

// Definición de una clase que representa un profesor.
class Profesor(nombre: String, edad: Int, asignatura: String) extends Persona(nombre, edad) {

  // Atributos de la clase Profesor.
  val asignatura: String = asignatura

  // Métodos de la clase Profesor.
  def darClase(): String = {
    "Estoy dando clase de " + asignatura + "."
  }
}

// Definición de una clase que representa una escuela.
class Escuela(nombre: String) {

  // Atributos de la clase Escuela.
  val nombre: String = nombre

  // Métodos de la clase Escuela.
  def matricularEstudiante(estudiante: Estudiante): Unit = {
    println("El estudiante " + estudiante.nombre + " ha sido matriculado en la escuela " + nombre + ".")
  }

  def contratarProfesor(profesor: Profesor): Unit = {
    println("El profesor " + profesor.nombre + " ha sido contratado en la escuela " + nombre + ".")
  }
}

// Creación de objetos de las clases Persona, Estudiante, Profesor y Escuela.
val persona1 = new Persona("Juan", 20)
val estudiante1 = new Estudiante("María", 18, "Informática")
val profesor1 = new Profesor("Pedro", 30, "Matemáticas")
val escuela1 = new Escuela("Colegio San José")

// Invocación de los métodos de los objetos creados.
println(persona1.saludar())
println(estudiante1.estudiar())
println(profesor1.darClase())
escuela1.matricularEstudiante(estudiante1)
escuela1.contratarProfesor(profesor1)
```

Explicación del código:

* La clase `Persona` define los atributos `nombre` y `edad`, y el método `saludar()`.
* La clase `Estudiante` extiende la clase `Persona` y define el atributo `carrera` y el método `estudiar()`.
* La clase `Profesor` extiende la clase `Persona` y define el atributo `asignatura` y el método `darClase()`.
* La clase `Escuela` define el atributo `nombre` y los métodos `matricularEstudiante()` y `contratarProfesor()`.
* Se crean objetos de las clases `Persona`, `Estudiante`, `Profesor` y `Escuela`.
* Se invocan los métodos de los objetos creados.