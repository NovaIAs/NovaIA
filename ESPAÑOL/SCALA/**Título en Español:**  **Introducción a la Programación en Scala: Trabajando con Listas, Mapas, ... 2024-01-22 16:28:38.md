```scala
// Paquete de la aplicación
package es.mipaquete

// Clase principal
object MiAplicacion {

  // Método principal
  def main(args: Array[String]): Unit = {

    // Creamos una lista de números enteros
    val listaNumeros = List(1, 2, 3, 4, 5)

    // Iteramos sobre la lista de números e imprimimos cada uno de ellos
    for (numero <- listaNumeros) {
      println(numero)
    }

    // Creamos un mapa de claves de tipo String y valores de tipo Int
    val mapaClavesValores = Map("uno" -> 1, "dos" -> 2, "tres" -> 3)

    // Iteramos sobre el mapa de claves y valores e imprimimos cada uno de ellos
    for ((clave, valor) <- mapaClavesValores) {
      println(clave + " -> " + valor)
    }

    // Creamos una función que recibe un número entero y devuelve su cuadrado
    def cuadrado(numero: Int): Int = {
      numero * numero
    }

    // Llamamos a la función cuadrado con el número 5 y imprimimos el resultado
    println(cuadrado(5))

    // Creamos una clase que representa a una persona
    class Persona(nombre: String, edad: Int) {

      // Propiedades de la clase
      val nombre: String = nombre
      val edad: Int = edad

      // Método de la clase que devuelve el nombre de la persona
      def getNombre(): String = {
        nombre
      }

      // Método de la clase que devuelve la edad de la persona
      def getEdad(): Int = {
        edad
      }
    }

    // Creamos un objeto de la clase Persona
    val persona = new Persona("Juan", 25)

    // Imprimimos el nombre y la edad de la persona
    println("Nombre: " + persona.nombre)
    println("Edad: " + persona.edad)
  }
}
```

**Explicación del código:**

* El código está escrito en Scala, un lenguaje de programación orientado a objetos y funcional.
* El código crea una lista de números enteros, un mapa de claves y valores, una función que recibe un número entero y devuelve su cuadrado, una clase que representa a una persona y un objeto de la clase Persona.
* El código itera sobre la lista de números enteros y el mapa de claves y valores e imprime cada uno de ellos.
* El código llama a la función cuadrado con el número 5 y imprime el resultado.
* El código crea un objeto de la clase Persona y imprime el nombre y la edad de la persona.