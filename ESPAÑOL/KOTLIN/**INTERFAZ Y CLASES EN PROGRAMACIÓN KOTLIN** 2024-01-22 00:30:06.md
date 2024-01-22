```kotlin
// Definición de la interfaz "Persona"
interface Persona {
    var nombre: String
    var edad: Int
    fun hablar(): String
}

// Clase "Estudiante" que implementa la interfaz "Persona"
class Estudiante(override var nombre: String, override var edad: Int) : Persona {
    override fun hablar(): String {
        return "Soy un estudiante y mi nombre es $nombre."
    }
}

// Clase "Profesor" que implementa la interfaz "Persona"
class Profesor(override var nombre: String, override var edad: Int) : Persona {
    override fun hablar(): String {
        return "Soy un profesor y mi nombre es $nombre."
    }
}

// Función "crearPersona" que crea una instancia de "Persona" en función del tipo especificado
fun crearPersona(tipo: String, nombre: String, edad: Int): Persona {
    return when (tipo) {
        "Estudiante" -> Estudiante(nombre, edad)
        "Profesor" -> Profesor(nombre, edad)
        else -> throw IllegalArgumentException("Tipo de persona no válido.")
    }
}

// Función "imprimirPersonas" que imprime los datos de una lista de "Persona"
fun imprimirPersonas(personas: List<Persona>) {
    for (persona in personas) {
        println("Nombre: ${persona.nombre}, Edad: ${persona.edad}, Hablando: ${persona.hablar()}")
    }
}

// Función principal
fun main(args: Array<String>) {
    // Creación de una lista de "Persona"
    val personas = mutableListOf<Persona>()
    personas.add(crearPersona("Estudiante", "Juan", 20))
    personas.add(crearPersona("Profesor", "María", 35))
    personas.add(crearPersona("Estudiante", "Pedro", 22))

    // Impresión de los datos de la lista de "Persona"
    imprimirPersonas(personas)
}
```

Explicación del código:

* La interfaz "Persona" define las propiedades y métodos comunes a todas las personas, como el nombre, la edad y el método "hablar()".
* Las clases "Estudiante" y "Profesor" implementan la interfaz "Persona" y proporcionan implementaciones específicas para los métodos de la interfaz.
* La función "crearPersona" crea una instancia de "Persona" en función del tipo especificado (Estudiante o Profesor) y los valores proporcionados para el nombre y la edad.
* La función "imprimirPersonas" imprime los datos de una lista de "Persona", incluyendo el nombre, la edad y el resultado de llamar al método "hablar()" para cada persona.
* La función principal crea una lista de "Persona" agregando instancias de "Estudiante" y "Profesor", y luego llama a la función "imprimirPersonas" para imprimir los datos de la lista.

Este código demuestra los siguientes conceptos de Kotlin:

* Interfaces
* Clases
* Funciones
* Polimorfismo
* Colecciones