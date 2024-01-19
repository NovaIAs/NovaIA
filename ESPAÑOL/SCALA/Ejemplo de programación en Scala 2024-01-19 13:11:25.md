```scala
// Clase Main que contiene el método main
class Main {
  def main(args: Array[String]): Unit = {
    // Este es un comentario de una sola línea en Scala.

    /*
      Este es un comentario de varias líneas en Scala.
      Se pueden utilizar para documentar el código o para deshabilitar temporalmente el código.
    */

    // Declaramos una variable llamada "nombre" de tipo String y le asignamos el valor "Juan".
    val nombre = "Juan"

    // Declaramos una variable llamada "edad" de tipo Int y le asignamos el valor 25.
    val edad = 25

    // Imprimimos en la consola el valor de las variables "nombre" y "edad".
    println(s"Nombre: $nombre")
    println(s"Edad: $edad")

    // Definimos una función llamada "suma" que recibe dos argumentos de tipo Int y devuelve un Int.
    def suma(a: Int, b: Int): Int = {
      return a + b
    }

    // Llamamos a la función "suma" y pasamos los valores 10 y 20 como argumentos.
    val resultado = suma(10, 20)

    // Imprimimos en la consola el valor de la variable "resultado".
    println(s"Resultado: $resultado")

    // Creamos una lista de números enteros.
    val numeros = List(1, 2, 3, 4, 5)

    // Imprimimos en la consola los elementos de la lista.
    numeros.foreach(println)

    // Creamos un mapa de claves y valores.
    val mapa = Map("nombre" -> "Juan", "edad" -> 25)

    // Imprimimos en la consola las claves y los valores del mapa.
    mapa.foreach(println)

    // Definimos una clase llamada "Persona" con dos propiedades: "nombre" y "edad".
    class Persona(nombre: String, edad: Int) {
      // Constructor de la clase "Persona".
      def this() = this("Sin nombre", 0)

      // Propiedades de la clase "Persona".
      var nombre: String = nombre
      var edad: Int = edad

      // Método de la clase "Persona" que imprime el nombre y la edad de la persona.
      def mostrar(): Unit = {
        println(s"$nombre, $edad")
      }
    }

    // Creamos un objeto de la clase "Persona".
    val persona = new Persona("Juan", 25)

    // Llamamos al método "mostrar()" del objeto "persona".
    persona.mostrar()

    // Creamos un objeto de la clase "Persona" utilizando el constructor por defecto.
    val persona2 = new Persona()

    // Llamamos al método "mostrar()" del objeto "persona2".
    persona2.mostrar()

    // Creamos un objeto de la clase "Persona" utilizando el constructor por defecto y asignando valores a las propiedades.
    val persona3 = new Persona {
      nombre = "María"
      edad = 30
    }

    // Llamamos al método "mostrar()" del objeto "persona3".
    persona3.mostrar()
  }
}
```

Explicación del código:

* En primer lugar, se define la clase `Main` que contiene el método `main`.
* A continuación, se declaran dos variables `nombre` y `edad` y se les asignan los valores `"Juan"` y `25` respectivamente.
* Después, se imprime en la consola el valor de estas dos variables.
* Seguidamente, se define una función llamada `suma` que recibe dos argumentos de tipo `Int` y devuelve un `Int`. Esta función suma los dos argumentos y devuelve el resultado.
* Entonces, se llama a la función `suma` pasándole los valores `10` y `20` como argumentos. El resultado de la función se almacena en la variable `resultado`.
* A continuación, se imprime en la consola el valor de la variable `resultado`.
* Luego, se crea una lista de números enteros llamada `numeros` y se imprime en la consola el valor de cada elemento de la lista.
* Después, se crea un mapa de claves y valores llamado `mapa` y se imprime en la consola la clave y el valor de cada elemento del mapa.
* A continuación, se define una clase llamada `Persona` con dos propiedades: `nombre` y `edad`. Esta clase tiene un método llamado `mostrar()` que imprime el nombre y la edad de la persona.
* Seguidamente, se crea un objeto de la clase `Persona` utilizando el constructor por defecto y asignando valores a las propiedades.
* Entonces, se llama al método `mostrar()` del objeto `persona`.
* Después, se crea otro objeto de la clase `Persona` utilizando el constructor por defecto.
* A continuación, se llama al método `mostrar()` del objeto `persona2`.
* Finalmente, se crea un tercer objeto de la clase `Persona` utilizando el constructor por defecto y asignando valores a las propiedades.
* Por último, se llama al método `mostrar()` del objeto `persona3`.