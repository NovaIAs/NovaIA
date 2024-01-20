```kotlin
// Crear una clase `Persona` con propiedades `nombre`, `apellido` y `edad`.
class Persona(val nombre: String, val apellido: String, val edad: Int) {

    // Crear una función `saludar()` que retorne un saludo personalizado.
    fun saludar(): String {
        return "Hola, mi nombre es $nombre $apellido y tengo $edad años."
    }
}

// Crear una lista de personas.
val personas = listOf(
    Persona("Juan", "García", 25),
    Persona("María", "López", 30),
    Persona("Pedro", "Sánchez", 35)
)

// Crear una función `imprimirPersonas()` que imprima los nombres de las personas en la lista.
fun imprimirPersonas(personas: List<Persona>) {
    for (persona in personas) {
        println(persona.nombre)
    }
}

// Crear una función `ordenarPersonasPorEdad()` que ordene la lista de personas por edad.
fun ordenarPersonasPorEdad(personas: List<Persona>): List<Persona> {
    return personas.sortedBy { persona -> persona.edad }
}

// Crear una función `filtrarPersonasPorNombre()` que filtre la lista de personas por nombre.
fun filtrarPersonasPorNombre(personas: List<Persona>, nombre: String): List<Persona> {
    return personas.filter { persona -> persona.nombre == nombre }
}

// Invocar las funciones y mostrar los resultados.
imprimirPersonas(personas)
println()
imprimirPersonas(ordenarPersonasPorEdad(personas))
println()
imprimirPersonas(filtrarPersonasPorNombre(personas, "Juan"))

```

Explicación del código:

* La clase `Persona` representa una persona con propiedades `nombre`, `apellido` y `edad`.

* La función `saludar()` devuelve un saludo personalizado para cada persona.

* La lista `personas` contiene tres objetos `Persona`.

* La función `imprimirPersonas()` imprime los nombres de las personas en la lista.

* La función `ordenarPersonasPorEdad()` ordena la lista de personas por edad.

* La función `filtrarPersonasPorNombre()` filtra la lista de personas por nombre.

* Las funciones se invocan y los resultados se muestran en la consola.