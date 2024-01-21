```scala
// Definición de una clase Persona con propiedades y métodos
class Persona(nombre: String, apellidos: String, edad: Int) {

  // Propiedades
  var nombre: String = nombre
  var apellidos: String = apellidos
  var edad: Int = edad

  // Métodos
  def saludar(): String = s"Hola, mi nombre es $nombre $apellidos y tengo $edad años."

}

// Definición de una clase Estudiante que hereda de la clase Persona
class Estudiante(nombre: String, apellidos: String, edad: Int, carrera: String) extends Persona(nombre, apellidos, edad) {

  // Propiedades
  var carrera: String = carrera

  // Métodos
  def estudiar(): String = s"Estoy estudiando la carrera de $carrera."

}

// Definición de una clase Profesor que hereda de la clase Persona
class Profesor(nombre: String, apellidos: String, edad: Int, asignatura: String) extends Persona(nombre, apellidos, edad) {

  // Propiedades
  var asignatura: String = asignatura

  // Métodos
  def enseñar(): String = s"Estoy enseñando la asignatura de $asignatura."

}

// Creación de una instancia de la clase Estudiante
val estudiante1 = new Estudiante("Juan", "Pérez", 20, "Ingeniería Informática")

// Creación de una instancia de la clase Profesor
val profesor1 = new Profesor("María", "García", 40, "Matemáticas")

// Impresión de los datos del estudiante y del profesor
println(estudiante1.saludar())
println(estudiante1.estudiar())
println(profesor1.saludar())
println(profesor1.enseñar())
```

Explicación del código:

* Se definen las clases `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` tiene propiedades y métodos comunes a todas las personas, como el nombre, los apellidos y la edad.
* La clase `Estudiante` hereda de la clase `Persona` y añade propiedades y métodos específicos de los estudiantes, como la carrera.
* La clase `Profesor` hereda de la clase `Persona` y añade propiedades y métodos específicos de los profesores, como la asignatura.
* Se crean instancias de las clases `Estudiante` y `Profesor`.
* Se imprimen los datos del estudiante y del profesor.