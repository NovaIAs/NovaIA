```kotlin
// Importamos las bibliotecas necesarias
import java.util.*
import kotlin.collections.ArrayList

// Definimos una clase llamada "Persona" con sus atributos y métodos
class Persona(var nombre: String, var edad: Int, var ocupacion: String) {
    // Método para obtener el nombre de la persona
    fun obtenerNombre(): String {
        return nombre
    }

    // Método para obtener la edad de la persona
    fun obtenerEdad(): Int {
        return edad
    }

    // Método para obtener la ocupación de la persona
    fun obtenerOcupacion(): String {
        return ocupacion
    }
}

// Definimos una clase llamada "Ciudad" con sus atributos y métodos
class Ciudad(var nombre: String, var poblacion: Int, var alcalde: Persona) {
    // Método para obtener el nombre de la ciudad
    fun obtenerNombre(): String {
        return nombre
    }

    // Método para obtener la población de la ciudad
    fun obtenerPoblacion(): Int {
        return poblacion
    }

    // Método para obtener el alcalde de la ciudad
    fun obtenerAlcalde(): Persona {
        return alcalde
    }
}

// Definimos una clase llamada "Pais" con sus atributos y métodos
class Pais(var nombre: String, var capital: Ciudad, var presidente: Persona) {
    // Método para obtener el nombre del país
    fun obtenerNombre(): String {
        return nombre
    }

    // Método para obtener la capital del país
    fun obtenerCapital(): Ciudad {
        return capital
    }

    // Método para obtener el presidente del país
    fun obtenerPresidente(): Persona {
        return presidente
    }
}

// Definimos una función para crear una lista de personas
fun crearListaPersonas(): ArrayList<Persona> {
    // Creamos una lista de personas
    val listaPersonas = ArrayList<Persona>()

    // Añadimos algunas personas a la lista
    listaPersonas.add(Persona("Juan", 25, "Ingeniero"))
    listaPersonas.add(Persona("María", 30, "Doctora"))
    listaPersonas.add(Persona("Pedro", 40, "Abogado"))

    // Devolvemos la lista de personas
    return listaPersonas
}

// Definimos una función para crear una lista de ciudades
fun crearListaCiudades(): ArrayList<Ciudad> {
    // Creamos una lista de ciudades
    val listaCiudades = ArrayList<Ciudad>()

    // Añadimos algunas ciudades a la lista
    listaCiudades.add(Ciudad("Madrid", 3200000, Persona("José", 50, "Alcalde")))
    listaCiudades.add(Ciudad("Barcelona", 1600000, Persona("Ana", 45, "Alcaldesa")))
    listaCiudades.add(Ciudad("Valencia", 800000, Persona("Pedro", 60, "Alcalde")))

    // Devolvemos la lista de ciudades
    return listaCiudades
}

// Definimos una función para crear una lista de países
fun crearListaPaises(): ArrayList<Pais> {
    // Creamos una lista de países
    val listaPaises = ArrayList<Pais>()

    // Añadimos algunos países a la lista
    listaPaises.add(Pais("España", Ciudad("Madrid", 3200000, Persona("José", 50, "Alcalde")), Persona("Felipe", 70, "Presidente")))
    listaPaises.add(Pais("Francia", Ciudad("París", 2200000, Persona("Anne", 40, "Alcaldesa")), Persona("Emmanuel", 60, "Presidente")))
    listaPaises.add(Pais("Alemania", Ciudad("Berlín", 3700000, Persona("Klaus", 55, "Alcalde")), Persona("Frank", 65, "Presidente")))

    // Devolvemos la lista de países
    return listaPaises
}

// Definimos una función principal
fun main(args: Array<String>) {
    // Creamos una lista de personas
    val listaPersonas = crearListaPersonas()

    // Creamos una lista de ciudades
    val listaCiudades = crearListaCiudades()

    // Creamos una lista de países
    val listaPaises = crearListaPaises()

    // Imprimimos la lista de personas
    println("Lista de personas:")
    for (persona in listaPersonas) {
        println("Nombre: ${persona.obtenerNombre()}")
        println("Edad: ${persona.obtenerEdad()}")
        println("Ocupación: ${persona.obtenerOcupacion()}")
        println()
    }

    // Imprimimos la lista de ciudades
    println("Lista de ciudades:")
    for (ciudad in listaCiudades) {
        println("Nombre: ${ciudad.obtenerNombre()}")
        println("Población: ${ciudad.obtenerPoblacion()}")
        println("Alcalde: ${ciudad.obtenerAlcalde().obtenerNombre()}")
        println()
    }

    // Imprimimos la lista de países
    println("Lista de países:")
    for (pais in listaPaises) {
        println("Nombre: ${pais.obtenerNombre()}")
        println("Capital: ${pais.obtenerCapital().obtenerNombre()}")
        println("Presidente: ${pais.obtenerPresidente().obtenerNombre()}")
        println()
    }
}
```

Explicación del código:

* **Clase Persona:** La clase `Persona` define los atributos y métodos de una persona. Los atributos son `nombre`, `edad` y `ocupacion`, y los métodos son `obtenerNombre()`, `obtenerEdad()` y `obtenerOcupacion()`.
* **Clase Ciudad:** La clase `Ciudad` define los atributos y métodos de una ciudad. Los atributos son `nombre`, `poblacion` y `alcalde`, y los métodos son `obtenerNombre()`, `obtenerPoblacion()` y `obtenerAlcalde()`.
* **Clase Pais:** La clase `Pais` define los atributos y métodos de un país. Los atributos son `nombre`, `capital` y `presidente`, y los métodos son `obtenerNombre()`, `obtenerCapital()` y `obtenerPresidente()`.
* **Función crearListaPersonas():** Esta función crea una lista de personas y la devuelve.
* **Función crearListaCiudades():** Esta función crea una lista de ciudades y la devuelve.
* **Función crearListaPaises():** Esta función crea una lista de países y la devuelve.
* **Función main():** Esta es la función principal del programa. Crea una lista de personas, una lista de ciudades y una lista de países, y luego las imprime.