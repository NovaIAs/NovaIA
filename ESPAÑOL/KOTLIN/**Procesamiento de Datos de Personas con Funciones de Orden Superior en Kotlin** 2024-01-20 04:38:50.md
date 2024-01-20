```kotlin
// Creamos una clase Persona con sus datos básicos
data class Persona(val nombre: String, val edad: Int)

// Definimos una función para calcular la edad promedio de una lista de personas
fun edadPromedio(personas: List<Persona>): Double {
    return personas.sumByDouble { it.edad } / personas.size
}

// Función para encontrar a la persona más joven de una lista
fun personaMasJoven(personas: List<Persona>): Persona? {
    return personas.minByOrNull { it.edad }
}

// Función para encontrar a la persona más vieja de una lista
fun personaMasVieja(personas: List<Persona>): Persona? {
    return personas.maxByOrNull { it.edad }
}

// Función para encontrar a todas las personas mayores de una edad dada
fun personasMayoresDe(personas: List<Persona>, edad: Int): List<Persona> {
    return personas.filter { it.edad > edad }
}

// Función para encontrar a todas las personas menores de una edad dada
fun personasMenoresDe(personas: List<Persona>, edad: Int): List<Persona> {
    return personas.filter { it.edad < edad }
}

// Función principal del programa
fun main(args: Array<String>) {
    // Creamos una lista de personas
    val personas = listOf(
        Persona("Juan", 25),
        Persona("María", 30),
        Persona("Pedro", 35),
        Persona("Ana", 40),
        Persona("José", 45)
    )

    // Calculamos la edad promedio de las personas
    val edadPromedio = edadPromedio(personas)
    println("Edad promedio: $edadPromedio años")

    // Buscamos a la persona más joven
    val personaMasJoven = personaMasJoven(personas)
    println("Persona más joven: ${personaMasJoven?.nombre} (${personaMasJoven?.edad} años)")

    // Buscamos a la persona más vieja
    val personaMasVieja = personaMasVieja(personas)
    println("Persona más vieja: ${personaMasVieja?.nombre} (${personaMasVieja?.edad} años)")

    // Buscamos a las personas mayores de 35 años
    val personasMayoresDe35 = personasMayoresDe(personas, 35)
    println("Personas mayores de 35 años:")
    personasMayoresDe35.forEach { println("${it.nombre} (${it.edad} años)") }

    // Buscamos a las personas menores de 30 años
    val personasMenoresDe30 = personasMenoresDe(personas, 30)
    println("Personas menores de 30 años:")
    personasMenoresDe30.forEach { println("${it.nombre} (${it.edad} años)") }
}
```

Este código es un ejemplo de cómo utilizar funciones de orden superior en Kotlin para procesar y analizar datos de una lista de personas.

El código comienza definiendo una clase Persona con dos propiedades: nombre y edad.

Luego, define una función edadPromedio() que calcula la edad promedio de una lista de personas. La función utiliza la función sumByDouble() para sumar las edades de todas las personas y luego dividir el resultado por el número de personas en la lista.

A continuación, define una función personaMasJoven() que encuentra a la persona más joven de una lista de personas. La función utiliza la función minByOrNull() para encontrar a la persona con la edad más baja.

También define una función personaMasVieja() que encuentra a la persona más vieja de una lista de personas. La función utiliza la función maxByOrNull() para encontrar a la persona con la edad más alta.

Luego, define dos funciones personasMayoresDe() y personasMenoresDe() que encuentran todas las personas mayores o menores de una edad dada, respectivamente. Las funciones utilizan la función filter() para filtrar la lista de personas y devolver solo las personas que cumplen con la condición de edad.

Finalmente, el código define una función main() que crea una lista de personas y luego llama a las funciones definidas anteriormente para procesar y analizar los datos de la lista. La función main() imprime los resultados en la consola.