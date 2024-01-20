```kotlin
// Importamos las bibliotecas necesarias.
import java.util.*
import kotlin.system.exitProcess

// Creamos la clase principal de nuestro programa.
class Main {
    // Creamos el método `main` de nuestro programa.
    fun main(args: Array<String>) {
        // Creamos una variable para almacenar el nombre del usuario.
        var nombre: String? = null

        // Creamos un bucle `while` para preguntar al usuario su nombre.
        while (nombre == null) {
            // Preguntamos al usuario su nombre.
            print("¿Cuál es tu nombre? ")

            // Leemos la respuesta del usuario.
            val input = readLine()

            // Si el usuario ha introducido un nombre, lo almacenamos en la variable `nombre`.
            if (input != null) {
                nombre = input
            }
        }

        // Creamos un bucle `while` para mostrar un menú al usuario.
        while (true) {
            // Mostramos el menú al usuario.
            println("Menú:")
            println("1. Hola, [nombre del usuario]")
            println("2. ¿Cuál es la fecha de hoy?")
            println("3. ¿Cuál es la hora actual?")
            println("4. Salir")

            // Leemos la opción elegida por el usuario.
            val opcion = readLine()

            // Ejecutamos la acción correspondiente a la opción elegida por el usuario.
            when (opcion) {
                "1" -> {
                    // Mostramos un mensaje de saludo al usuario.
                    println("Hola, $nombre!")
                }
                "2" -> {
                    // Mostramos la fecha actual al usuario.
                    println("La fecha de hoy es ${Date().toString()}")
                }
                "3" -> {
                    // Mostramos la hora actual al usuario.
                    println("La hora actual es ${Date().toString()}")
                }
                "4" -> {
                    // Salimos del programa.
                    exitProcess(0)
                }
                else -> {
                    // Mostramos un mensaje de error al usuario.
                    println("Opción no válida")
                }
            }
        }
    }
}
```

**Explicación del código:**

* **Líneas 1-3:** Importamos las bibliotecas necesarias.
* **Líneas 5-14:** Creamos la clase principal de nuestro programa.
* **Líneas 16-26:** Creamos el método `main` de nuestro programa.
* **Líneas 18-25:** Creamos una variable para almacenar el nombre del usuario y pedimos al usuario su nombre.
* **Líneas 27-44:** Creamos un bucle `while` para mostrar un menú al usuario.
* **Líneas 29-38:** Mostramos el menú al usuario y leemos la opción elegida por el usuario.
* **Líneas 40-43:** Ejecutamos la acción correspondiente a la opción elegida por el usuario.
* **Líneas 45-47:** Mostramos un mensaje de error al usuario si la opción elegida no es válida.
* **Líneas 50-55:** Salimos del programa si el usuario elige la opción 4.