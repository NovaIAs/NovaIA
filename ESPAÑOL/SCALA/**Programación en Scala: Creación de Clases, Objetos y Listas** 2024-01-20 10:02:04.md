```scala
// Importamos las librerías necesarias
import scala.collection.mutable.ArrayBuffer
import scala.util.Random

// Definimos una clase Persona con sus atributos y métodos
class Persona(nombre: String, edad: Int) {
  // Atributos
  var nombre: String = nombre
  var edad: Int = edad

  // Métodos
  def saludar(): Unit = {
    println(s"Hola, mi nombre es $nombre y tengo $edad años.")
  }
}

// Definimos una clase Empresa con sus atributos y métodos
class Empresa(nombre: String, empleados: ArrayBuffer[Persona]) {
  // Atributos
  var nombre: String = nombre
  var empleados: ArrayBuffer[Persona] = empleados

  // Métodos
  def contratarEmpleado(persona: Persona): Unit = {
    empleados += persona
  }

  def despedirEmpleado(persona: Persona): Unit = {
    empleados -= persona
  }
}

// Creamos una instancia de la clase Persona
val persona1 = new Persona("Juan", 25)
val persona2 = new Persona("María", 30)
val persona3 = new Persona("Pedro", 35)

// Creamos una instancia de la clase Empresa
val empresa1 = new Empresa("ACME", ArrayBuffer(persona1, persona2, persona3))

// Añadimos un empleado a la empresa
empresa1.contratarEmpleado(new Persona("Ana", 20))

// Despedimos un empleado de la empresa
empresa1.despedirEmpleado(persona3)

// Imprimimos los empleados de la empresa
println(s"Empleados de la empresa ${empresa1.nombre}:")
empresa1.empleados.foreach(_.saludar())

// Creamos una lista de números aleatorios
val numeros = ArrayBuffer[Int]()
for (i <- 1 to 10) {
  numeros += Random.nextInt(100)
}

// Imprimimos la lista de números aleatorios
println(s"Números aleatorios:")
numeros.foreach(println)

// Calculamos la media de los números aleatorios
val media = numeros.sum / numeros.length
println(s"Media de los números aleatorios: $media")
```

Explicación del código:

* Definimos dos clases, Persona y Empresa, con sus atributos y métodos correspondientes.
* Creamos instancias de las clases Persona y Empresa y realizamos operaciones sobre ellas, como contratar y despedir empleados.
* Creamos una lista de números aleatorios y calculamos su media.

El código es complejo porque utiliza conceptos avanzados de programación en Scala, como clases, objetos, métodos, listas, bucles y funciones lambda. También utiliza la librería `Random` para generar números aleatorios.