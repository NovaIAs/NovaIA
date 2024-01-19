**Tarea**: Crear un algoritmo en Swift que permita a los usuarios crear y administrar una lista de tareas. El algoritmo debe permitir a los usuarios agregar nuevas tareas, eliminar tareas existentes y marcar tareas como completadas.

**Código**:

```swift
// Importar las bibliotecas necesarias
import Foundation

// Crear una clase Tarea
class Tarea {
    var nombre: String
    var descripcion: String?
    var completada: Bool

    // Inicializador de la clase Tarea
    init(nombre: String, descripcion: String?, completada: Bool) {
        self.nombre = nombre
        self.descripcion = descripcion
        self.completada = completada
    }
}

// Crear una clase ListaDeTareas
class ListaDeTareas {
    var tareas: [Tarea]

    // Inicializador de la clase ListaDeTareas
    init() {
        tareas = []
    }

    // Método para agregar una tarea a la lista
    func agregarTarea(tarea: Tarea) {
        tareas.append(tarea)
    }

    // Método para eliminar una tarea de la lista
    func eliminarTarea(tarea: Tarea) {
        if let index = tareas.firstIndex(where: { $0 === tarea }) {
            tareas.remove(at: index)
        }
    }

    // Método para marcar una tarea como completada
    func marcarTareaComoCompletada(tarea: Tarea) {
        if let index = tareas.firstIndex(where: { $0 === tarea }) {
            tareas[index].completada = true
        }
    }

    // Método para obtener todas las tareas de la lista
    func obtenerTodasLasTareas() -> [Tarea] {
        return tareas
    }

    // Método para obtener todas las tareas completadas de la lista
    func obtenerTodasLasTareasCompletadas() -> [Tarea] {
        return tareas.filter({ $0.completada })
    }

    // Método para obtener todas las tareas pendientes de la lista
    func obtenerTodasLasTareasPendientes() -> [Tarea] {
        return tareas.filter({ !$0.completada })
    }
}

// Crear una instancia de la clase ListaDeTareas
let listaDeTareas = ListaDeTareas()

// Agregar algunas tareas a la lista
let tarea1 = Tarea(nombre: "Comprar leche", descripcion: "Comprar un litro de leche entera", completada: false)
let tarea2 = Tarea(nombre: "Hacer la cama", descripcion: "Hacer la cama antes de salir de casa", completada: true)
let tarea3 = Tarea(nombre: "Lavar los platos", descripcion: "Lavar los platos después de la cena", completada: false)

listaDeTareas.agregarTarea(tarea: tarea1)
listaDeTareas.agregarTarea(tarea: tarea2)
listaDeTareas.agregarTarea(tarea: tarea3)

// Obtener todas las tareas de la lista
let todasLasTareas = listaDeTareas.obtenerTodasLasTareas()

// Mostrar todas las tareas en la consola
for tarea in todasLasTareas {
    print("Tarea: \(tarea.nombre)")
    if let descripcion = tarea.descripcion {
        print("Descripción: \(descripcion)")
    }
    print("Completada: \(tarea.completada)")
    print("-------------------------------")
}

// Marcar la tarea 1 como completada
listaDeTareas.marcarTareaComoCompletada(tarea: tarea1)

// Obtener todas las tareas completadas de la lista
let todasLasTareasCompletadas = listaDeTareas.obtenerTodasLasTareasCompletadas()

// Mostrar todas las tareas completadas en la consola
for tarea in todasLasTareasCompletadas {
    print("Tarea: \(tarea.nombre)")
    if let descripcion = tarea.descripcion {
        print("Descripción: \(descripcion)")
    }
    print("Completada: \(tarea.completada)")
    print("-------------------------------")
}

// Eliminar la tarea 3 de la lista
listaDeTareas.eliminarTarea(tarea: tarea3)

// Obtener todas las tareas pendientes de la lista
let todasLasTareasPendientes = listaDeTareas.obtenerTodasLasTareasPendientes()

// Mostrar todas las tareas pendientes en la consola
for tarea in todasLasTareasPendientes {
    print("Tarea: \(tarea.nombre)")
    if let descripcion = tarea.descripcion {
        print("Descripción: \(descripcion)")
    }
    print("Completada: \(tarea.completada)")
    print("-------------------------------")
}
```

**Explicación del código:**

* La clase `Tarea` representa una tarea individual. Tiene propiedades para almacenar el nombre de la tarea, la descripción de la tarea y si la tarea está completada o no.
* La clase `ListaDeTareas` representa una lista de tareas. Tiene propiedades para almacenar las tareas de la lista y métodos para agregar, eliminar y marcar tareas como completadas.
* La instancia `listaDeTareas` de la clase `ListaDeTareas` se utiliza para almacenar y administrar las tareas.
* Las tareas `tarea1`, `tarea2` y `tarea3` se crean y se agregan a la lista de tareas.
* Se muestran todas las tareas de la lista en la consola.
* Se marca la tarea `tarea1` como completada.
* Se muestran todas las tareas completadas de la lista en la consola.
* Se elimina la tarea `tarea3` de la lista.
* Se muestran todas las tareas pendientes de la lista en la consola.