```kotlin
// Estructura de Datos Personalizada

// Clase Persona con propiedades nombre, edad y género
class Persona(val nombre: String, val edad: Int, val género: String)

// Lista de Personas para almacenar objetos Persona
val personas: MutableList<Persona> = mutableListOf()

// Función para agregar una nueva persona a la lista
fun agregarPersona(persona: Persona) {
    personas.add(persona)
}

// Función para eliminar una persona de la lista por su nombre
fun eliminarPersona(nombre: String) {
    personas.removeIf { it.nombre == nombre }
}

// Función para buscar una persona en la lista por su nombre y devolverla si se encuentra
fun buscarPersona(nombre: String): Persona? {
    return personas.find { it.nombre == nombre }
}

// Función para filtrar la lista de personas por género y devolver una lista filtrada
fun filtrarPorGénero(género: String): List<Persona> {
    return personas.filter { it.género == género }
}

// Función para calcular la edad promedio de las personas en la lista
fun edadPromedio(): Double {
    return personas.sumByDouble { it.edad } / personas.size
}

// Función para imprimir los nombres de las personas en la lista
fun imprimirNombres() {
    personas.forEach { println(it.nombre) }
}

// Clase principal con el punto de entrada del programa
class Main {

    // Función principal
    fun main(args: Array<String>) {

        // Crear personas y agregarlas a la lista
        agregarPersona(Persona("Juan", 25, "Masculino"))
        agregarPersona(Persona("María", 30, "Femenino"))
        agregarPersona(Persona("Pedro", 20, "Masculino"))

        // Eliminar una persona de la lista
        eliminarPersona("Juan")

        // Buscar una persona en la lista
        val personaBuscada = buscarPersona("María")
        if (personaBuscada != null) {
            println("Persona encontrada: ${personaBuscada.nombre}")
        } else {
            println("Persona no encontrada")
        }

        // Filtrar la lista de personas por género
        val personasMasculinas = filtrarPorGénero("Masculino")
        println("Personas masculinas:")
        imprimirNombres(personasMasculinas)

        // Calcular la edad promedio de las personas en la lista
        val edadPromedio = edadPromedio()
        println("Edad promedio: $edadPromedio")

        // Imprimir los nombres de las personas en la lista
        println("Lista de personas:")
        imprimirNombres()
    }
}
```

Explicación del código:

* Definimos una clase Persona con las propiedades nombre, edad y género.
* Creamos una lista mutable llamada personas para almacenar objetos Persona.
* Definimos varias funciones para agregar, eliminar, buscar y filtrar personas en la lista.
* También definimos una función para calcular la edad promedio de las personas en la lista.
* Finalmente, creamos la clase principal Main con el punto de entrada del programa.
* En la función main, creamos algunas personas, las agregamos a la lista, eliminamos una persona, buscamos una persona, filtramos la lista por género, calculamos la edad promedio e imprimimos los nombres de las personas en la lista.

Este código es más complejo que el ejemplo anterior porque incluye varias funciones y operaciones diferentes. Sin embargo, sigue siendo relativamente fácil de entender y modificar.