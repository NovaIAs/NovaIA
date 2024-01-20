```kotlin
// Definir una clase de datos para representar un objeto Persona.
data class Persona(val nombre: String, val edad: Int, val profesion: String)

// Crear una lista de objetos Persona.
val personas = listOf(
    Persona("Juan", 25, "Ingeniero"),
    Persona("María", 30, "Médica"),
    Persona("Pedro", 35, "Abogado"),
    Persona("Ana", 40, "Profesora"),
    Persona("Luis", 45, "Empresario")
)

// Crear una función para obtener el nombre y la edad de una persona.
fun obtenerNombreYEdad(persona: Persona): Pair<String, Int> {
    return Pair(persona.nombre, persona.edad)
}

// Crear una función para obtener la profesión de una persona.
fun obtenerProfesion(persona: Persona): String {
    return persona.profesion
}

// Crear una función para obtener una lista de los nombres de las personas.
fun obtenerNombres(personas: List<Persona>): List<String> {
    return personas.map { it.nombre }
}

// Crear una función para obtener una lista de las edades de las personas.
fun obtenerEdades(personas: List<Persona>): List<Int> {
    return personas.map { it.edad }
}

// Crear una función para obtener una lista de las profesiones de las personas.
fun obtenerProfesiones(personas: List<Persona>): List<String> {
    return personas.map { it.profesion }
}

// Crear una función para obtener una lista de pares de nombres y edades de las personas.
fun obtenerNombresYEdades(personas: List<Persona>): List<Pair<String, Int>> {
    return personas.map { obtenerNombreYEdad(it) }
}

// Crear una función para obtener una lista de pares de nombres y profesiones de las personas.
fun obtenerNombresYProfesiones(personas: List<Persona>): List<Pair<String, String>> {
    return personas.map { Pair(it.nombre, it.profesion) }
}

// Crear una función para obtener una lista de pares de edades y profesiones de las personas.
fun obtenerEdadesYProfesiones(personas: List<Persona>): List<Pair<Int, String>> {
    return personas.map { Pair(it.edad, it.profesion) }
}

// Imprimir los nombres y las edades de las personas.
println("Nombres y edades de las personas:")
personas.forEach { println("${it.nombre} tiene ${it.edad} años") }

// Imprimir las profesiones de las personas.
println("Profesiones de las personas:")
personas.forEach { println("${it.nombre} es ${it.profesion}") }

// Imprimir la lista de nombres de las personas.
println("Nombres de las personas:")
println(obtenerNombres(personas))

// Imprimir la lista de edades de las personas.
println("Edades de las personas:")
println(obtenerEdades(personas))

// Imprimir la lista de profesiones de las personas.
println("Profesiones de las personas:")
println(obtenerProfesiones(personas))

// Imprimir la lista de pares de nombres y edades de las personas.
println("Nombres y edades de las personas:")
println(obtenerNombresYEdades(personas))

// Imprimir la lista de pares de nombres y profesiones de las personas.
println("Nombres y profesiones de las personas:")
println(obtenerNombresYProfesiones(personas))

// Imprimir la lista de pares de edades y profesiones de las personas.
println("Edades y profesiones de las personas:")
println(obtenerEdadesYProfesiones(personas))