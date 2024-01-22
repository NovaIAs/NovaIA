```swift
// Este código en SWIFT crea una aplicación sencilla para gestionar una lista de tareas.

// Definimos una estructura llamada Tarea que representa cada tarea que se añade a la lista.
struct Tarea {
    var nombre: String
    var completada: Bool
}

// Creamos una clase llamada ListaDeTareas que gestiona la lista de tareas.
class ListaDeTareas {
    // Creamos un array para almacenar las tareas.
    var tareas: [Tarea] = []

    // Añadimos una nueva tarea a la lista.
    func añadirTarea(tarea: Tarea) {
        tareas.append(tarea)
    }

    // Eliminamos una tarea de la lista.
    func eliminarTarea(tarea: Tarea) {
        if let index = tareas.firstIndex(of: tarea) {
            tareas.remove(at: index)
        }
    }

    // Marcamos una tarea como completada.
    func marcarCompletada(tarea: Tarea) {
        if let index = tareas.firstIndex(of: tarea) {
            tareas[index].completada = true
        }
    }

    // Obtenemos el número de tareas completadas.
    func obtenerTareasCompletadas() -> Int {
        return tareas.filter({ $0.completada }).count
    }
}

// Creamos una instancia de la clase ListaDeTareas.
let listaDeTareas = ListaDeTareas()

// Creamos algunas tareas y las añadimos a la lista.
let tarea1 = Tarea(nombre: "Hacer la compra", completada: false)
let tarea2 = Tarea(nombre: "Ir al gimnasio", completada: false)
let tarea3 = Tarea(nombre: "Estudiar para el examen", completada: false)
listaDeTareas.añadirTarea(tarea: tarea1)
listaDeTareas.añadirTarea(tarea: tarea2)
listaDeTareas.añadirTarea(tarea: tarea3)

// Mostramos la lista de tareas.
print("Lista de tareas:")
for tarea in listaDeTareas.tareas {
    print("\(tarea.nombre) - \(tarea.completada ? "Completada" : "Pendiente")")
}

// Marcamos una tarea como completada.
listaDeTareas.marcarCompletada(tarea: tarea1)

// Mostramos de nuevo la lista de tareas.
print("\nLista de tareas después de marcar una tarea como completada:")
for tarea in listaDeTareas.tareas {
    print("\(tarea.nombre) - \(tarea.completada ? "Completada" : "Pendiente")")
}

// Obtenemos el número de tareas completadas.
let tareasCompletadas = listaDeTareas.obtenerTareasCompletadas()
print("\nNúmero de tareas completadas:", tareasCompletadas)
```

Este código en SWIFT crea una aplicación sencilla para gestionar una lista de tareas.

* La estructura `Tarea` representa cada tarea que se añade a la lista. Tiene dos propiedades: `nombre` y `completada`.
* La clase `ListaDeTareas` gestiona la lista de tareas. Tiene un método para añadir una nueva tarea, un método para eliminar una tarea, un método para marcar una tarea como completada y un método para obtener el número de tareas completadas.
* Creamos una instancia de la clase `ListaDeTareas` y añadimos algunas tareas a la lista.
* Mostramos la lista de tareas.
* Marcamos una tarea como completada.
* Mostramos de nuevo la lista de tareas.
* Obtenemos el número de tareas completadas.

Este código es más complejo que el anterior porque utiliza estructuras, clases y métodos. También muestra cómo se pueden utilizar los métodos para gestionar una lista de tareas.