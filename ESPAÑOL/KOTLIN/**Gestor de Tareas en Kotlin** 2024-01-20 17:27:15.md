```kotlin
// Aplicación para gestionar una lista de tareas
//
// Importamos las clases y librerías necesarias
import java.util.*
import java.io.File
import kotlin.system.exitProcess

// Definimos la clase Tarea, que representa una tarea individual
class Tarea(val descripcion: String, val fecha: Date, val prioridad: Int) {

    // Función para formatear la fecha de la tarea
    fun formatearFecha(): String {
        val formateador = SimpleDateFormat("dd/MM/yyyy")
        return formateador.format(fecha)
    }
}

// Definimos la clase ListaTareas, que gestiona una lista de tareas
class ListaTareas {

    // La lista de tareas
    private val tareas = mutableListOf<Tarea>()

    // Función para añadir una nueva tarea a la lista
    fun añadirTarea(tarea: Tarea) {
        tareas.add(tarea)
    }

    // Función para eliminar una tarea de la lista
    fun eliminarTarea(tarea: Tarea) {
        tareas.remove(tarea)
    }

    // Función para buscar una tarea por su descripción
    fun buscarTarea(descripcion: String): Tarea? {
        return tareas.find { it.descripcion == descripcion }
    }

    // Función para imprimir la lista de tareas
    fun imprimirLista() {
        println("Lista de tareas:")
        tareas.forEach { println("$it\n") }
    }
}

// Función principal del programa
fun main(args: Array<String>) {

    // Creamos una instancia de la clase ListaTareas
    val listaTareas = ListaTareas()

    // Leemos las tareas del fichero "tareas.txt"
    val tareas = File("tareas.txt").readLines()

    // Convertimos las tareas en objetos de la clase Tarea
    val tareasConvertidas = tareas.map {
        val partes = it.split(",")
        Tarea(partes[0], SimpleDateFormat("dd/MM/yyyy").parse(partes[1]), partes[2].toInt())
    }

    // Añadimos las tareas a la lista de tareas
    tareasConvertidas.forEach { listaTareas.añadirTarea(it) }

    // Imprimimos la lista de tareas
    listaTareas.imprimirLista()

    // Leemos el comando del usuario
    var comando = ""
    while (comando != "salir") {
        print("Comando: ")
        comando = readLine()!!

        // Procesamos el comando
        when (comando) {

            // Añadir una nueva tarea
            "añadir" -> {
                print("Descripción: ")
                val descripcion = readLine()!!
                print("Fecha: ")
                val fecha = SimpleDateFormat("dd/MM/yyyy").parse(readLine()!!)
                print("Prioridad: ")
                val prioridad = readLine()!!.toInt()
                val tarea = Tarea(descripcion, fecha, prioridad)
                listaTareas.añadirTarea(tarea)
                println("Tarea añadida correctamente")
            }

            // Eliminar una tarea
            "eliminar" -> {
                print("Descripción: ")
                val descripcion = readLine()!!
                val tarea = listaTareas.buscarTarea(descripcion)
                if (tarea != null) {
                    listaTareas.eliminarTarea(tarea)
                    println("Tarea eliminada correctamente")
                } else {
                    println("Tarea no encontrada")
                }
            }

            // Buscar una tarea
            "buscar" -> {
                print("Descripción: ")
                val descripcion = readLine()!!
                val tarea = listaTareas.buscarTarea(descripcion)
                if (tarea != null) {
                    println("Tarea encontrada:")
                    println("$tarea\n")
                } else {
                    println("Tarea no encontrada")
                }
            }

            // Salir del programa
            "salir" -> {
                println("Saliendo del programa...")
                exitProcess(0)
            }

            // Comando no reconocido
            else -> println("Comando no reconocido")
        }
    }
}
```

Este código es complejo y consta de varias partes:

* **Clase Tarea:** Representa una tarea individual con una descripción, una fecha y una prioridad.
* **Clase ListaTareas:** Gestiona una lista de tareas y proporciona métodos para añadir, eliminar y buscar tareas.
* **Función principal:** Lee las tareas de un fichero, las convierte en objetos de la clase Tarea y las añade a la lista de tareas. Imprime la lista de tareas y permite al usuario añadir, eliminar y buscar tareas.

El código utiliza la librería `java.util` para trabajar con fechas y la librería `java.io` para leer el fichero de tareas. También utiliza la función `exitProcess` de la librería `kotlin.system` para salir del programa.

El código es modular y está bien organizado en clases y funciones. Utiliza comentarios para explicar el código y hacerlo más fácil de entender.