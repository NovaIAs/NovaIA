```kotlin
// Creación de una clase Persona con propiedades nombre y edad.
class Persona(var nombre: String, var edad: Int) {

    // Método para obtener el nombre de la persona.
    fun getNombre(): String {
        return nombre
    }

    // Método para obtener la edad de la persona.
    fun getEdad(): Int {
        return edad
    }

    // Método para imprimir los datos de la persona.
    fun imprimirDatos() {
        println("Nombre: $nombre")
        println("Edad: $edad")
    }
}

// Creación de una clase Estudiante que hereda de la clase Persona.
class Estudiante(nombre: String, edad: Int, var carrera: String) : Persona(nombre, edad) {

    // Método para obtener la carrera del estudiante.
    fun getCarrera(): String {
        return carrera
    }

    // Método para imprimir los datos del estudiante.
    override fun imprimirDatos() {
        super.imprimirDatos()
        println("Carrera: $carrera")
    }
}

// Creación de una clase Profesor que hereda de la clase Persona.
class Profesor(nombre: String, edad: Int, var asignatura: String) : Persona(nombre, edad) {

    // Método para obtener la asignatura del profesor.
    fun getAsignatura(): String {
        return asignatura
    }

    // Método para imprimir los datos del profesor.
    override fun imprimirDatos() {
        super.imprimirDatos()
        println("Asignatura: $asignatura")
    }
}

// Creación de una función para crear una lista de personas.
fun crearListaPersonas(): List<Persona> {
    val personas = mutableListOf<Persona>()

    personas.add(Persona("Juan", 20))
    personas.add(Persona("María", 25))
    personas.add(Estudiante("Pedro", 22, "Ingeniería"))
    personas.add(Profesor("Ana", 30, "Matemáticas"))

    return personas
}

// Creación de una función para filtrar la lista de personas por edad.
fun filtrarPersonasPorEdad(personas: List<Persona>, edadMinima: Int, edadMaxima: Int): List<Persona> {
    val personasFiltradas = mutableListOf<Persona>()

    for (persona in personas) {
        if (persona.getEdad() >= edadMinima && persona.getEdad() <= edadMaxima) {
            personasFiltradas.add(persona)
        }
    }

    return personasFiltradas
}

// Creación de una función para imprimir los datos de una lista de personas.
fun imprimirDatosPersonas(personas: List<Persona>) {
    for (persona in personas) {
        persona.imprimirDatos()
        println()
    }
}

// Creación de una función principal.
fun main(args: Array<String>) {
    // Creación de una lista de personas.
    val personas = crearListaPersonas()

    // Filtrar la lista de personas por edad.
    val personasFiltradas = filtrarPersonasPorEdad(personas, 20, 25)

    // Imprimir los datos de la lista de personas filtradas.
    imprimirDatosPersonas(personasFiltradas)
}
```

Explicación del código:

* Se crean dos clases, `Persona` y `Estudiante`, que heredan de la clase `Persona`.
* Se crea una función `crearListaPersonas()` que crea una lista de objetos `Persona`.
* Se crea una función `filtrarPersonasPorEdad()` que filtra la lista de personas por edad.
* Se crea una función `imprimirDatosPersonas()` que imprime los datos de una lista de personas.
* Se crea una función `main()` que crea una lista de personas, la filtra por edad e imprime los datos de la lista filtrada.

El código anterior demuestra el uso de clases, herencia, funciones, listas y bucles en Kotlin.