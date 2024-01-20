```kotlin

// Importación de librerías necesarias
import kotlinx.coroutines.*
import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.atomic.AtomicInteger


// Clase para representar una tarea
class Tarea(val id: Int, val peso: Int, val duración: Long) {
    // Atributos adicionales
    var asignadoA: String? = null
    var fechaInicio: Long = 0L
    var fechaFin: Long = 0L
}


// Clase para representar un recurso
class Recurso(val id: Int, val tipo: String, val capacidad: Int) {
    // Atributos adicionales
    val tareasAsignadas = ConcurrentHashMap<Int, Tarea>()
}


// Clase para representar un trabajador
class Trabajador(val id: Int, val nombre: String, val recursosDisponibles: MutableMap<Int, Recurso>) {
    // Atributos adicionales
    val tareasAsignadas = ConcurrentHashMap<Int, Tarea>()
}


// Función para generar una lista de tareas
fun generarTareas(numTareas: Int): List<Tarea> {
    val tareas = mutableListOf<Tarea>()
    for (i in 1..numTareas) {
        tareas.add(Tarea(i, (1..10).random(), (1000..5000).random().toLong()))
    }
    return tareas
}


// Función para generar una lista de recursos
fun generarRecursos(numRecursos: Int): List<Recurso> {
    val recursos = mutableListOf<Recurso>()
    for (i in 1..numRecursos) {
        recursos.add(Recurso(i, "Tipo $i", (1..10).random()))
    }
    return recursos
}


// Función para generar una lista de trabajadores
fun generarTrabajadores(numTrabajadores: Int): List<Trabajador> {
    val trabajadores = mutableListOf<Trabajador>()
    for (i in 1..numTrabajadores) {
        trabajadores.add(Trabajador(i, "Trabajador $i", ConcurrentHashMap()))
    }
    return trabajadores
}

suspend fun asignarTareasATrabajadores(tareas: List<Tarea>, trabajadores: List<Trabajador>, recursos: List<Recurso>) {
    // Crear un alcance para administrar los coroutines
    val alcance = CoroutineScope(Dispatchers.IO)

    // Crear un coroutine para cada tarea
    tareas.forEach { tarea ->
        alcance.launch {
            // Obtener los recursos necesarios para la tarea
            val recursosNecesarios = tarea.peso
            val recursosDisponibles = trabajadores.map { it.recursosDisponibles.values }.flatten()

            // Buscar los recursos disponibles que satisfagan las necesidades de la tarea
            val recursosAsignados = recursosDisponibles.filter { it.capacidad >= recursosNecesarios }

            // Asignar la tarea al trabajador con más recursos disponibles
            val trabajadorAsignado = trabajadores.maxBy { it.recursosDisponibles.values.sumOf { it.capacidad } }

            // Asignar la tarea al trabajador seleccionado
            trabajadorAsignado?.tareasAsignadas?.put(tarea.id, tarea)
            tarea.asignadoA = trabajadorAsignado?.nombre
            tarea.fechaInicio = System.currentTimeMillis()

            // Liberar los recursos asignados a la tarea
            recursosAsignados.forEach {
                it.tareasAsignadas.remove(tarea.id)
                trabajadorAsignado?.recursosDisponibles?.put(it.id, it)
            }
        }
    }
}

suspend fun ejecutarTareas(tareas: List<Tarea>) {
    // Crear un alcance para administrar los coroutines
    val alcance = CoroutineScope(Dispatchers.IO)

    // Crear un coroutine para cada tarea
    tareas.forEach { tarea ->
        alcance.launch {
            // Simular la ejecución de la tarea
            delay(tarea.duración)

            // Marcar la tarea como finalizada
            tarea.fechaFin = System.currentTimeMillis()
        }
    }
}

fun imprimirResultados(tareas: List<Tarea>, trabajadores: List<Trabajador>, recursos: List<Recurso>) {
    println("\nResultados:")

    println("\nTareas:")
    tareas.forEach { tarea ->
        println("Tarea ${tarea.id}: ${tarea.peso} | ${tarea.duración} | Asignada a ${tarea.asignadoA} | Fecha de inicio: ${tarea.fechaInicio} | Fecha de fin: ${tarea.fechaFin}")
    }

    println("\nTrabajadores:")
    trabajadores.forEach { trabajador ->
        println("Trabajador ${trabajador.id}: ${trabajador.nombre} | Tareas asignadas: ${trabajador.tareasAsignadas.keys}")
    }

    println("\nRecursos:")
    recursos.forEach { recurso ->
        println("Recurso ${recurso.id}: ${recurso.tipo} | Capacidad: ${recurso.capacidad} | Tareas asignadas: ${recurso.tareasAsignadas.keys}")
    }
}

// Función principal
fun main(args: Array<String>) = runBlocking {
    // Generar listas de tareas, recursos y trabajadores
    val tareas = generarTareas(10)
    val recursos = generarRecursos(5)
    val trabajadores = generarTrabajadores(3)

    // Asignar tareas a los trabajadores
    asignarTareasATrabajadores(tareas, trabajadores, recursos)

    // Ejecutar las tareas
    ejecutarTareas(tareas)

    // Imprimir los resultados
    imprimirResultados(tareas, trabajadores, recursos)
}

```

Explicación del código:

1. **Generación de listas de tareas, recursos y trabajadores:**
   - Se utilizan funciones generadoras para crear listas de tareas, recursos y trabajadores con valores aleatorios.


2. **Asignación de recursos y tareas a los trabajadores:**
   - La función `asignarTareasATrabajadores` se ejecuta en paralelo utilizando coroutines para asignar tareas a los trabajadores en función de la disponibilidad de recursos.


3. **Ejecución de las tareas:**
   - La función `ejecutarTareas` se ejecuta en paralelo utilizando coroutines para simular la ejecución de las tareas.


4. **Impresión de los resultados:**
   - La función `imprimirResultados` imprime los resultados de la asignación y ejecución de las tareas, incluyendo información sobre las tareas, los trabajadores y los recursos.


5. **Función principal:**
   - La función `main` es la función de entrada del programa. Ejecuta las funciones necesarias para generar, asignar y ejecutar las tareas, y luego imprime los resultados.