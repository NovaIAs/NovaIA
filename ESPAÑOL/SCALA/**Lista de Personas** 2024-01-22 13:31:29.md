```scala
// Clase Persona
class Persona(val nombre: String, val edad: Int) {

    // Método para obtener el nombre de la persona
    def getNombre: String = {
        nombre
    }

    // Método para obtener la edad de la persona
    def getEdad: Int = {
        edad
    }

    // Método para imprimir los datos de la persona
    def imprimirDatos(): Unit = {
        println("Nombre: " + nombre)
        println("Edad: " + edad)
    }
}

// Clase ListaPersonas
class ListaPersonas {

    // Lista de personas
    private var personas: List[Persona] = List()

    // Método para agregar una persona a la lista
    def agregarPersona(persona: Persona): Unit = {
        personas = persona :: personas
    }

    // Método para eliminar una persona de la lista
    def eliminarPersona(persona: Persona): Unit = {
        personas = personas.filter(p => p != persona)
    }

    // Método para obtener la lista de personas
    def getPersonas: List[Persona] = {
        personas
    }

    // Método para imprimir los datos de todas las personas en la lista
    def imprimirDatosPersonas(): Unit = {
        for (persona <- personas) {
            persona.imprimirDatos()
            println()
        }
    }
}

// Clase Main
object Main {

    def main(args: Array[String]): Unit = {

        // Crear una lista de personas
        val listaPersonas = new ListaPersonas()

        // Agregar algunas personas a la lista
        listaPersonas.agregarPersona(new Persona("Juan", 20))
        listaPersonas.agregarPersona(new Persona("María", 25))
        listaPersonas.agregarPersona(new Persona("Pedro", 30))

        // Eliminar una persona de la lista
        listaPersonas.eliminarPersona(new Persona("Juan", 20))

        // Imprimir los datos de todas las personas en la lista
        listaPersonas.imprimirDatosPersonas()
    }
}
```

Explicación del código:

* La clase `Persona` representa una persona con un nombre y una edad.
* La clase `ListaPersonas` representa una lista de personas.
* La clase `Main` es la clase principal que crea una lista de personas, agrega algunas personas a la lista, elimina una persona de la lista e imprime los datos de todas las personas en la lista.

El código crea una lista de personas y agrega algunas personas a la lista. Luego, elimina una persona de la lista y finalmente imprime los datos de todas las personas en la lista.

El código también utiliza la sintaxis de Scala para definir clases, métodos y variables. La sintaxis de Scala es similar a la de Java, pero tiene algunas características adicionales, como la posibilidad de utilizar caracteres Unicode en los identificadores y la posibilidad de utilizar funciones anónimas.