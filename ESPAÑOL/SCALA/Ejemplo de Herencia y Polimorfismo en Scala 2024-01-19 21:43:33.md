```scala
// Definimos la clase Persona
class Persona(nombre: String, edad: Int) {
  // Definimos los atributos de la clase Persona
  val nombre: String = nombre
  val edad: Int = edad

  // Definimos los métodos de la clase Persona
  def saludar(): Unit = {
    println(s"Hola, me llamo $nombre y tengo $edad años.")
  }

  def esMayorDeEdad(): Boolean = {
    return edad >= 18
  }
}

// Definimos la clase Estudiante, que hereda de la clase Persona
class Estudiante(nombre: String, edad: Int, matricula: String) extends Persona(nombre, edad) {
  // Definimos los atributos de la clase Estudiante
  val matricula: String = matricula

  // Definimos los métodos de la clase Estudiante
  def estudiar(): Unit = {
    println(s"Estoy estudiando.")
  }
}

// Definimos la clase Profesor, que hereda de la clase Persona
class Profesor(nombre: String, edad: Int, asignatura: String) extends Persona(nombre, edad) {
  // Definimos los atributos de la clase Profesor
  val asignatura: String = asignatura

  // Definimos los métodos de la clase Profesor
  def enseñar(): Unit = {
    println(s"Estoy enseñando $asignatura.")
  }
}

// Creamos un objeto de la clase Persona
val persona1 = new Persona("Juan", 25)

// Creamos un objeto de la clase Estudiante
val estudiante1 = new Estudiante("María", 20, "123456")

// Creamos un objeto de la clase Profesor
val profesor1 = new Profesor("Pedro", 40, "Matemáticas")

// Llamamos a los métodos de los objetos creados
persona1.saludar()
estudiante1.estudiar()
profesor1.enseñar()
```

Este código define tres clases: Persona, Estudiante y Profesor. Las clases Estudiante y Profesor heredan de la clase Persona.

La clase Persona tiene dos atributos: nombre y edad. También tiene dos métodos: saludar() y esMayorDeEdad().

La clase Estudiante tiene un atributo adicional: matricula. También tiene un método adicional: estudiar().

La clase Profesor tiene un atributo adicional: asignatura. También tiene un método adicional: enseñar().

En el código también se crean tres objetos: persona1, estudiante1 y profesor1. Estos objetos se utilizan para llamar a los métodos de las clases a las que pertenecen.

El código de ejemplo muestra cómo utilizar la herencia y el polimorfismo en Scala. La herencia nos permite crear nuevas clases que heredan de las clases existentes. El polimorfismo nos permite tratar los objetos de diferentes clases como si fueran objetos de una misma clase.