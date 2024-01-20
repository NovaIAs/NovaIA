```kotlin
// Importamos las librerías necesarias
import java.util.*
import kotlin.collections.ArrayList

// Definimos una clase `Persona` con propiedades y un constructor
class Persona(val nombre: String, val edad: Int) {
    // Esta función devuelve el nombre de la persona
    fun getNombre(): String {
        return nombre
    }

    // Esta función devuelve la edad de la persona
    fun getEdad(): Int {
        return edad
    }
}

// Definimos una clase `ListaPersonas` que contiene una lista de personas
class ListaPersonas {
    private val personas: MutableList<Persona> = ArrayList()

    // Esta función agrega una persona a la lista
    fun agregarPersona(persona: Persona) {
        personas.add(persona)
    }

    // Esta función devuelve la lista de personas
    fun getPersonas(): List<Persona> {
        return personas
    }
}

// Definimos una clase `Menu` con las opciones del menú
class Menu {
    private val opciones: Array<String> = arrayOf("Agregar persona", "Mostrar personas", "Salir")

    // Esta función muestra el menú y devuelve la opción seleccionada
    fun mostrarMenu(): Int {
        println("------------------------------------")
        println("MENÚ")
        println("------------------------------------")
        opciones.forEachIndexed { index, opcion -> println("${index + 1}. $opcion") }
        print("Elige una opción: ")
        return readLine()!!.toInt()
    }
}

// Esta función es el punto de entrada del programa
fun main(args: Array<String>) {
    // Creamos una lista de personas
    val listaPersonas = ListaPersonas()

    // Creamos un objeto `Menu` para mostrar el menú
    val menu = Menu()

    // Bucle principal del programa
    while (true) {
        // Mostramos el menú y obtenemos la opción seleccionada
        val opcion = menu.mostrarMenu()

        // Ejecutamos la acción correspondiente a la opción seleccionada
        when (opcion) {
            // Agregar persona
            1 -> {
                print("Nombre: ")
                val nombre = readLine()!!
                print("Edad: ")
                val edad = readLine()!!.toInt()
                listaPersonas.agregarPersona(Persona(nombre, edad))
            }

            // Mostrar personas
            2 -> {
                listaPersonas.getPersonas().forEach { persona -> println("${persona.getNombre()} - ${persona.getEdad()} años") }
            }

            // Salir
            3 -> {
                println("Gracias por usar el programa.")
                break
            }

            // Opción inválida
            else -> {
                println("Opción inválida.")
            }
        }
    }
}
```

Este código crea un programa en Kotlin que permite agregar y mostrar una lista de personas. El programa tiene un menú principal que muestra las opciones disponibles al usuario. El usuario puede elegir agregar una persona, mostrar la lista de personas o salir del programa.

El programa utiliza una clase `Persona` para almacenar los datos de cada persona. La clase tiene dos propiedades: `nombre` y `edad`. El programa también utiliza una clase `ListaPersonas` para almacenar la lista de personas. La clase tiene un método `agregarPersona()` para agregar una persona a la lista y un método `getPersonas()` para obtener la lista de personas.

El programa también utiliza una clase `Menu` para mostrar el menú principal. La clase tiene un método `mostrarMenu()` que muestra el menú y devuelve la opción seleccionada por el usuario.

El programa utiliza un bucle `while` para ejecutar el programa hasta que el usuario elija la opción de salir. En cada iteración del bucle, el programa muestra el menú y ejecuta la acción correspondiente a la opción seleccionada por el usuario.