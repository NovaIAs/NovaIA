```kotlin
// Crea un conjunto de datos de ejemplo.
val miLista = listOf("Hola", "Mundo", "!", "¿Cómo", "estás?")

// Utiliza una lambda para contar el número de palabras que contienen un carácter específico.
val contadorDeCaracteres = miLista.count { it.contains('ó') }

// Imprime el número de palabras que contienen el carácter específico.
println("El número de palabras que contienen el carácter 'ó' es: $contadorDeCaracteres")

// Crea una clase para representar una persona.
class Persona(val nombre: String, val apellido: String, val edad: Int) {
    // Define una función para obtener el nombre completo de la persona.
    fun obtenerNombreCompleto(): String {
        return "$nombre $apellido"
    }

    // Define una función para obtener la edad de la persona.
    fun obtenerEdad(): Int {
        return edad
    }
}

// Crea una lista de objetos Persona.
val miListaDePersonas = listOf(
    Persona("Juan", "Pérez", 25),
    Persona("María", "López", 30),
    Persona("Pedro", "García", 35)
)

// Utiliza una lambda para filtrar la lista de personas por edad.
val personasMayoresDe30 = miListaDePersonas.filter { it.obtenerEdad() > 30 }

// Imprime la lista de personas mayores de 30 años.
println("La lista de personas mayores de 30 años es:")
for (persona in personasMayoresDe30) {
    println(persona.obtenerNombreCompleto())
}

// Define una función para calcular el promedio de edad de una lista de personas.
fun calcularPromedioDeEdad(personas: List<Persona>): Double {
    // Suma las edades de todas las personas.
    val sumaDeEdades = personas.sumBy { it.obtenerEdad() }

    // Divide la suma de las edades por el número de personas.
    val promedioDeEdad = sumaDeEdades / personas.size

    // Devuelve el promedio de edad.
    return promedioDeEdad
}

// Calcula el promedio de edad de la lista de personas.
val promedioDeEdad = calcularPromedioDeEdad(miListaDePersonas)

// Imprime el promedio de edad.
println("El promedio de edad de la lista de personas es: $promedioDeEdad")
```

**Explicación:**

Este código es un ejemplo de código complejo en Kotlin. El código crea un conjunto de datos de ejemplo, utiliza una lambda para contar el número de palabras que contienen un carácter específico, crea una clase para representar una persona, crea una lista de objetos Persona, utiliza una lambda para filtrar la lista de personas por edad, imprime la lista de personas mayores de 30 años, define una función para calcular el promedio de edad de una lista de personas, calcula el promedio de edad de la lista de personas e imprime el promedio de edad.

El código utiliza una variedad de características de Kotlin, incluyendo lambdas, clases, listas y funciones. El código también es un ejemplo de cómo se puede utilizar Kotlin para resolver problemas complejos de forma concisa y clara.