```scala
// Definición de la clase principal del programa
object HolaMundo {

  // Definición de la función principal del programa
  def main(args: Array[String]): Unit = {

    // Impresión de un mensaje en la consola
    println("Hola mundo!")

    // Definición de una variable entera
    var edad: Int = 25

    // Definición de una variable decimal
    var altura: Double = 1.75

    // Definición de una variable booleana
    var casado: Boolean = false

    // Definición de una cadena de texto
    var nombre: String = "Juan"

    // Definición de un array de elementos enteros
    var edades: Array[Int] = Array(25, 30, 35, 40)

    // Definición de un mapa de pares clave-valor
    var alturas: Map[String, Double] = Map("Juan" -> 1.75, "María" -> 1.65, "Pedro" -> 1.80)

    // Definición de una lista de elementos enteros
    var salarios: List[Int] = List(1000, 2000, 3000, 4000)

    // Definición de una función que calcula el área de un círculo
    def areaCirculo(radio: Double): Double = {
      val pi = 3.14159265
      return pi * radio * radio
    }

    // Definición de una clase que representa a una persona
    class Persona(var nombre: String, var edad: Int) {

      // Definición del método toString de la clase Persona
      override def toString(): String = {
        return s"Nombre: $nombre, Edad: $edad"
      }
    }

    // Creación de una instancia de la clase Persona
    var persona1 = new Persona("Juan", 25)

    // Impresión de la información de la persona
    println(persona1)

    // Definición de una clase que representa a un perro
    class Perro(var nombre: String, var raza: String) {

      // Definición del método toString de la clase Perro
      override def toString(): String = {
        return s"Nombre: $nombre, Raza: $raza"
      }
    }

    // Creación de una instancia de la clase Perro
    var perro1 = new Perro("Firulais", "Pastor Alemán")

    // Impresión de la información del perro
    println(perro1)
  }
}
```

Explicación del código:

* El código define una clase principal llamada `HolaMundo` que contiene la función `main` que se ejecuta cuando se ejecuta el programa.
* La función `main` imprime un mensaje en la consola y luego define varias variables de diferentes tipos.
* También define un array, un mapa y una lista.
* Define una función que calcula el área de un círculo.
* Define dos clases: `Persona` y `Perro`, que representan a una persona y a un perro respectivamente.
* Crea instancias de las clases `Persona` y `Perro` y las imprime en la consola.

Este código es un ejemplo complejo de código en Scala que cubre una variedad de conceptos, como variables, tipos de datos, arrays, mapas, listas, funciones y clases.