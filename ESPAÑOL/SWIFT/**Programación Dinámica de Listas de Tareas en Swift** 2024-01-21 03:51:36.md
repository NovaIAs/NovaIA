```swift
// Código para crear una lista de tareas dinámica
// Importamos la clase UIKit para acceder a las vistas y controles.
import UIKit

// Creamos una clase que represente una tarea.
class Tarea {
    // Creamos una propiedad para almacenar el título de la tarea.
    var titulo: String

    // Creamos una propiedad para almacenar la descripción de la tarea.
    var descripcion: String

    // Creamos una propiedad para almacenar la fecha de creación de la tarea.
    var fechaCreacion: Date

    // Creamos una propiedad para almacenar la fecha límite de la tarea.
    var fechaLimite: Date

    // Creamos una propiedad para almacenar el estado de completitud de la tarea.
    var completada: Bool

    // Creamos un inicializador para la clase Tarea.
    init(titulo: String, descripcion: String, fechaCreacion: Date, fechaLimite: Date, completada: Bool) {
        self.titulo = titulo
        self.descripcion = descripcion
        self.fechaCreacion = fechaCreacion
        self.fechaLimite = fechaLimite
        self.completada = completada
    }
}

// Creamos una clase que represente una lista de tareas.
class ListaDeTareas {
    // Creamos una propiedad para almacenar la lista de tareas.
    var tareas: [Tarea]

    // Creamos un inicializador para la clase ListaDeTareas.
    init(tareas: [Tarea]) {
        self.tareas = tareas
    }

    // Creamos un método para añadir una tarea a la lista de tareas.
    func añadirTarea(tarea: Tarea) {
        self.tareas.append(tarea)
    }

    // Creamos un método para eliminar una tarea de la lista de tareas.
    func eliminarTarea(tarea: Tarea) {
        if let índice = self.tareas.firstIndex(where: { $0 === tarea }) {
            self.tareas.remove(at: índice)
        }
    }

    // Creamos un método para marcar una tarea como completada.
    func marcarCompletada(tarea: Tarea) {
        if let índice = self.tareas.firstIndex(where: { $0 === tarea }) {
            self.tareas[índice].completada = true
        }
    }

    // Creamos un método para desmarcar una tarea como completada.
    func desmarcarCompletada(tarea: Tarea) {
        if let índice = self.tareas.firstIndex(where: { $0 === tarea }) {
            self.tareas[índice].completada = false
        }
    }
}

// Creamos una instancia de la clase ListaDeTareas.
let listaDeTareas = ListaDeTareas(tareas: [])

// Creamos una tarea de ejemplo.
let tarea1 = Tarea(titulo: "Comprar leche", descripcion: "Necesitamos comprar leche para el desayuno.", fechaCreacion: Date(), fechaLimite: Date().addingTimeInterval(60 * 60 * 24), completada: false)

// Añadimos la tarea a la lista de tareas.
listaDeTareas.añadirTarea(tarea: tarea1)

// Creamos otra tarea de ejemplo.
let tarea2 = Tarea(titulo: "Lavar el coche", descripcion: "Necesitamos lavar el coche antes de la próxima reunión.", fechaCreacion: Date(), fechaLimite: Date().addingTimeInterval(60 * 60 * 24 * 2), completada: false)

// Añadimos la tarea a la lista de tareas.
listaDeTareas.añadirTarea(tarea: tarea2)

// Creamos una vista de tabla para mostrar la lista de tareas.
let vistaTabla = UITableView()

// Configuramos la vista de tabla.
vistaTabla.dataSource = self
vistaTabla.delegate = self

// Mostramos la vista de tabla en la ventana principal.
vistaTabla.frame = UIScreen.main.bounds
self.view.addSubview(vistaTabla)

// Creamos una función para obtener el número de tareas en la lista de tareas.
func tableView(_ tableView: UITableView, numberOfRowsInSection section: Int) -> Int {
    return listaDeTareas.tareas.count
}

// Creamos una función para obtener la celda correspondiente a cada tarea.
func tableView(_ tableView: UITableView, cellForRowAt indexPath: IndexPath) -> UITableViewCell {
    // Creamos una celda reutilizable.
    let celda = tableView.dequeueReusableCell(withIdentifier: "TareaCelda", for: indexPath)

    // Obtenemos la tarea correspondiente a la celda.
    let tarea = listaDeTareas.tareas[indexPath.row]

    // Configuramos la celda con la información de la tarea.
    celda.textLabel?.text = tarea.titulo
    celda.detailTextLabel?.text = tarea.descripcion

    // Devolvemos la celda.
    return celda
}

// Creamos una función para manejar la pulsación de una celda.
func tableView(_ tableView: UITableView, didSelectRowAt indexPath: IndexPath) {
    // Obtenemos la tarea correspondiente a la celda.
    let tarea = listaDeTareas.tareas[indexPath.row]

    // Si la tarea está completada, la desmarcamos como completada.
    if tarea.completada {
        listaDeTareas.desmarcarCompletada(tarea: tarea)
    } else {
        // Si la tarea no está completada, la marcamos como completada.
        listaDeTareas.marcarCompletada(tarea: tarea)
    }

    // Recargamos la vista de tabla para actualizar la información.
    tableView.reloadData()