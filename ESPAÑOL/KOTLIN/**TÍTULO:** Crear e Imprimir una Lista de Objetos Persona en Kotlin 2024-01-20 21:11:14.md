```kotlin
// Definición de una clase con propiedades y métodos
class Persona(val nombre: String, val edad: Int) {

    // Método para obtener el nombre de la persona
    fun getNombre(): String {
        return nombre
    }

    // Método para obtener la edad de la persona
    fun getEdad(): Int {
        return edad
    }

    // Método para calcular la edad de la persona en un año
    fun getEdadEnUnAño(): Int {
        return edad + 1
    }

    // Método para imprimir la información de la persona
    fun imprimirInformacion() {
        println("Nombre: $nombre")
        println("Edad: $edad")
    }
}

// Función para crear una lista de personas
fun crearListaDePersonas(): List<Persona> {
    val listaPersonas = listOf(
        Persona("Juan", 20),
        Persona("María", 25),
        Persona("Pedro", 30)
    )
    return listaPersonas
}

// Función para imprimir la información de una lista de personas
fun imprimirListaDePersonas(listaPersonas: List<Persona>) {
    for (persona in listaPersonas) {
        persona.imprimirInformacion()
        println()
    }
}

// Función principal del programa
fun main(args: Array<String>) {

    // Creación de una lista de personas
    val listaPersonas = crearListaDePersonas()

    // Impresión de la información de la lista de personas
    imprimirListaDePersonas(listaPersonas)
}

```

Explicación del código:

* La clase `Persona` define un objeto persona con dos propiedades: `nombre` y `edad`. También define varios métodos para acceder y modificar estas propiedades, así como un método para imprimir la información de la persona.
* La función `crearListaDePersonas()` crea una lista de objetos persona con nombres y edades aleatorios.
* La función `imprimirListaDePersonas()` imprime la información de una lista de objetos persona en consola.
* La función `main()` es la función principal del programa. Crea una lista de objetos persona y luego imprime la información de la lista en consola.