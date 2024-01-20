```scala
// Definición de un objeto con una clase anidada
object MiObjeto {

  // Clase anidada que representa una persona
  class Persona(nombre: String, edad: Int) {
    // Atributos de la clase Persona
    val nombre: String = nombre
    val edad: Int = edad

    // Método que devuelve el nombre y la edad de la persona
    def obtenerNombreEdad(): String = {
      s"$nombre tiene $edad años"
    }
  }

  // Función que devuelve una lista de personas
  def obtenerListaPersonas(): List[Persona] = {
    List(
      new Persona("Juan", 25),
      new Persona("María", 30),
      new Persona("Pedro", 35)
    )
  }

  // Función principal del programa
  def main(args: Array[String]): Unit = {
    // Obtenemos la lista de personas
    val listaPersonas = obtenerListaPersonas()

    // Recorremos la lista de personas e imprimimos sus nombres y edades
    listaPersonas.foreach { persona =>
      println(persona.obtenerNombreEdad())
    }
  }
}
```

**Explicación del código:**

* El código define un objeto `MiObjeto` que contiene una clase anidada `Persona`.
* La clase `Persona` tiene dos atributos: `nombre` y `edad`.
* También tiene un método `obtenerNombreEdad()` que devuelve el nombre y la edad de la persona.
* La función `obtenerListaPersonas()` devuelve una lista de personas.
* La función `main()` es la función principal del programa. Obtén la lista de personas y recorre la lista para imprimir sus nombres y edades.