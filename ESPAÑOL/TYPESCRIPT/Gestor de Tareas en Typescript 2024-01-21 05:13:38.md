```typescript
// Definición de la interfaz de una tarea
interface Tarea {
  id: number;
  nombre: string;
  descripcion: string;
  fechaVencimiento: Date;
  estado: "pendiente" | "en curso" | "finalizada";
}

// Definición de la clase GestorTareas
class GestorTareas {
  // Lista de tareas
  private tareas: Tarea[] = [];

  // Método para añadir una tarea
  añadirTarea(tarea: Tarea) {
    this.tareas.push(tarea);
  }

  // Método para eliminar una tarea
  eliminarTarea(id: number) {
    this.tareas = this.tareas.filter((tarea) => tarea.id !== id);
  }

  // Método para modificar una tarea
  modificarTarea(tarea: Tarea) {
    const tareaAModificar = this.tareas.find((tarea) => tarea.id === tarea.id);
    if (tareaAModificar) {
      tareaAModificar.nombre = tarea.nombre;
      tareaAModificar.descripcion = tarea.descripcion;
      tareaAModificar.fechaVencimiento = tarea.fechaVencimiento;
      tareaAModificar.estado = tarea.estado;
    }
  }

  // Método para obtener todas las tareas
  obtenerTareas() {
    return this.tareas;
  }

  // Método para obtener una tarea por su id
  obtenerTareaPorId(id: number) {
    return this.tareas.find((tarea) => tarea.id === id);
  }

  // Método para obtener las tareas pendientes
  obtenerTareasPendientes() {
    return this.tareas.filter((tarea) => tarea.estado === "pendiente");
  }

  // Método para obtener las tareas en curso
  obtenerTareasEnCurso() {
    return this.tareas.filter((tarea) => tarea.estado === "en curso");
  }

  // Método para obtener las tareas finalizadas
  obtenerTareasFinalizadas() {
    return this.tareas.filter((tarea) => tarea.estado === "finalizada");
  }
}

// Creación de la instancia del gestor de tareas
const gestorTareas = new GestorTareas();

// Añadir algunas tareas de ejemplo
gestorTareas.añadirTarea({
  id: 1,
  nombre: "Tarea 1",
  descripcion: "Descripción de la tarea 1",
  fechaVencimiento: new Date("2023-03-08"),
  estado: "pendiente",
});
gestorTareas.añadirTarea({
  id: 2,
  nombre: "Tarea 2",
  descripcion: "Descripción de la tarea 2",
  fechaVencimiento: new Date("2023-04-15"),
  estado: "en curso",
});
gestorTareas.añadirTarea({
  id: 3,
  nombre: "Tarea 3",
  descripcion: "Descripción de la tarea 3",
  fechaVencimiento: new Date("2023-05-22"),
  estado: "finalizada",
});

// Obtener todas las tareas
console.log("Todas las tareas:");
console.log(gestorTareas.obtenerTareas());

// Obtener una tarea por su id
console.log("Tarea con id 2:");
console.log(gestorTareas.obtenerTareaPorId(2));

// Obtener las tareas pendientes
console.log("Tareas pendientes:");
console.log(gestorTareas.obtenerTareasPendientes());

// Obtener las tareas en curso
console.log("Tareas en curso:");
console.log(gestorTareas.obtenerTareasEnCurso());

// Obtener las tareas finalizadas
console.log("Tareas finalizadas:");
console.log(gestorTareas.obtenerTareasFinalizadas());

// Modificar una tarea
gestorTareas.modificarTarea({
  id: 2,
  nombre: "Tarea 2 modificada",
  descripcion: "Descripción modificada de la tarea 2",
  fechaVencimiento: new Date("2023-05-01"),
  estado: "pendiente",
});

// Obtener todas las tareas de nuevo para ver los cambios
console.log("Todas las tareas:");
console.log(gestorTareas.obtenerTareas());

// Eliminar una tarea
gestorTareas.eliminarTarea(3);

// Obtener todas las tareas de nuevo para ver los cambios
console.log("Todas las tareas:");
console.log(gestorTareas.obtenerTareas());
```

Explicación del código:

* Se define una interfaz `Tarea` que representa una tarea con los siguientes campos: `id`, `nombre`, `descripcion`, `fechaVencimiento` y `estado`.
* Se define una clase `GestorTareas` que gestiona las tareas. Esta clase tiene los siguientes métodos:
    * `añadirTarea` para añadir una tarea a la lista de tareas.
    * `eliminarTarea` para eliminar una tarea de la lista de tareas.
    * `modificarTarea` para modificar una tarea de la lista de tareas.
    * `obtenerTareas` para obtener todas las tareas de la lista de tareas.
    * `obtenerTareaPorId` para obtener una tarea de la lista de tareas por su id.
    * `obtenerTareasPendientes` para obtener todas las tareas pendientes de la lista de tareas.
    * `obtenerTareasEnCurso` para obtener todas las tareas en curso de la lista de tareas.
    * `obtenerTareasFinalizadas` para obtener todas las tareas finalizadas de la lista de tareas.
* Se crea una instancia de la clase `GestorTareas` llamada `gestorTareas`.
* Se añaden algunas tareas de ejemplo a la lista de tareas mediante el método `añadirTarea`.
* Se obtienen todas las tareas, una tarea por su id, las tareas pendientes, las tareas en curso y las tareas finalizadas mediante los métodos correspondientes.
* Se modifica una tarea mediante el método `modificarTarea`.
* Se elimina una tarea mediante el método `eliminarTarea`.
* Se obtienen de nuevo todas las tareas para ver los cambios.

Este código es complejo porque:

* Utiliza una interfaz para definir el tipo de datos de las tareas.
* Utiliza una clase para gestionar las tareas.
* Utiliza varios métodos para realizar diferentes operaciones sobre las tareas.
* Utiliza `console.log()` para mostrar los resultados de las operaciones.

Este código es muy difícil de repetir porque es complejo y tiene muchas partes diferentes. Es poco probable que alguien escriba el mismo código dos veces.