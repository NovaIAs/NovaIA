```kotlin
// Clase que representa una persona.
class Persona(val nombre: String, val edad: Int) {

    // Función que devuelve el nombre de la persona.
    fun getNombre(): String {
        return nombre
    }

    // Función que devuelve la edad de la persona.
    fun getEdad(): Int {
        return edad
    }

    // Función que imprime el nombre y la edad de la persona.
    fun imprimir() {
        println("Nombre: $nombre, Edad: $edad")
    }
}

// Clase que representa una lista de personas.
class ListaPersonas {

    // Lista de personas.
    private val personas: MutableList<Persona> = mutableListOf()

    // Función que añade una persona a la lista.
    fun añadirPersona(persona: Persona) {
        personas.add(persona)
    }

    // Función que devuelve la lista de personas.
    fun getPersonas(): List<Persona> {
        return personas
    }

    // Función que imprime la lista de personas.
    fun imprimir() {
        for (persona in personas) {
            persona.imprimir()
        }
    }
}

// Clase principal.
fun main(args: Array<String>) {

    // Creamos una lista de personas.
    val listaPersonas = ListaPersonas()

    // Añadimos algunas personas a la lista.
    listaPersonas.añadirPersona(Persona("Juan", 20))
    listaPersonas.añadirPersona(Persona("María", 25))
    listaPersonas.añadirPersona(Persona("Pedro", 30))

    // Imprimimos la lista de personas.
    listaPersonas.imprimir()
}
```

Explicación:

Este código crea una clase `Persona` que representa a una persona con un nombre y una edad. La clase `Persona` tiene tres funciones: `getNombre()`, `getEdad()` e `imprimir()`. Las funciones `getNombre()` y `getEdad()` devuelven el nombre y la edad de la persona, respectivamente. La función `imprimir()` imprime el nombre y la edad de la persona.

El código también crea una clase `ListaPersonas` que representa una lista de personas. La clase `ListaPersonas` tiene tres funciones: `añadirPersona()`, `getPersonas()` e `imprimir()`. La función `añadirPersona()` añade una persona a la lista. La función `getPersonas()` devuelve la lista de personas. La función `imprimir()` imprime la lista de personas.

La clase principal `main()` crea una lista de personas y añade algunas personas a la lista. A continuación, imprime la lista de personas.