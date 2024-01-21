```kotlin
// Crea una clase Persona con propiedades nombre, apellido y edad
class Persona(val nombre: String, val apellido: String, val edad: Int) {

    // Define una función para obtener el nombre completo de la persona
    fun obtenerNombreCompleto(): String {
        return "$nombre $apellido"
    }

    // Define una función para obtener la edad de la persona
    fun obtenerEdad(): Int {
        return edad
    }

    // Define una función para imprimir los datos de la persona
    fun imprimirDatos() {
        println("Nombre: $nombre")
        println("Apellido: $apellido")
        println("Edad: $edad")
    }
}

// Crea una clase Estudiante que hereda de la clase Persona
class Estudiante(nombre: String, apellido: String, edad: Int, val curso: String) : Persona(nombre, apellido, edad) {

    // Define una función para obtener el curso del estudiante
    fun obtenerCurso(): String {
        return curso
    }

    // Define una función para imprimir los datos del estudiante
    fun imprimirDatos() {
        super.imprimirDatos()
        println("Curso: $curso")
    }
}

// Crea una clase Profesor que hereda de la clase Persona
class Profesor(nombre: String, apellido: String, edad: Int, val materia: String) : Persona(nombre, apellido, edad) {

    // Define una función para obtener la materia del profesor
    fun obtenerMateria(): String {
        return materia
    }

    // Define una función para imprimir los datos del profesor
    fun imprimirDatos() {
        super.imprimirDatos()
        println("Materia: $materia")
    }
}

// Crea una lista de personas
val personas = listOf(
    Persona("Juan", "García", 20),
    Persona("María", "Pérez", 25),
    Estudiante("Pedro", "López", 18, "Informática"),
    Profesor("Ana", "Fernández", 30, "Matemáticas")
)

// Itera sobre la lista de personas e imprime los datos de cada persona
for (persona in personas) {
    persona.imprimirDatos()
    println()
}
```

Explicación del código:

* La clase `Persona` define las propiedades `nombre`, `apellido` y `edad`. También define funciones para obtener el nombre completo, la edad y para imprimir los datos de la persona.
* La clase `Estudiante` hereda de la clase `Persona` y define una propiedad adicional `curso`. También define una función para obtener el curso del estudiante y una función para imprimir los datos del estudiante.
* La clase `Profesor` hereda de la clase `Persona` y define una propiedad adicional `materia`. También define una función para obtener la materia del profesor y una función para imprimir los datos del profesor.
* La lista `personas` contiene objetos de las clases `Persona`, `Estudiante` y `Profesor`.
* El ciclo `for` itera sobre la lista `personas` e imprime los datos de cada persona.

Este código demuestra la herencia en Kotlin y cómo se pueden crear clases que heredan de otras clases y añaden propiedades y métodos adicionales.