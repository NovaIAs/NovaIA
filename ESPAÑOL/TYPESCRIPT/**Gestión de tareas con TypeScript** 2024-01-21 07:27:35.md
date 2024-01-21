```typescript
// Definir una clase para representar una tarea
class Tarea {
    constructor(titulo, descripcion, fechaLimite, prioridad) {
        this.titulo = titulo;
        this.descripcion = descripcion;
        this.fechaLimite = fechaLimite;
        this.prioridad = prioridad;
        this.completada = false;
    }

    // Marcar la tarea como completada
    completar() {
        this.completada = true;
    }
}

// Definir una clase para representar una lista de tareas
class ListaTareas {
    constructor() {
        this.tareas = [];
    }

    // Añadir una tarea a la lista
    añadirTarea(tarea) {
        this.tareas.push(tarea);
    }

    // Obtener todas las tareas de la lista
    obtenerTareas() {
        return this.tareas;
    }

    // Obtener todas las tareas completadas de la lista
    obtenerTareasCompletadas() {
        return this.tareas.filter((tarea) => tarea.completada);
    }

    // Obtener todas las tareas pendientes de la lista
    obtenerTareasPendientes() {
        return this.tareas.filter((tarea) => !tarea.completada);
    }

    // Eliminar una tarea de la lista
    eliminarTarea(tarea) {
        const indice = this.tareas.indexOf(tarea);
        if (indice > -1) {
            this.tareas.splice(indice, 1);
        }
    }
}

// Crear una instancia de la clase ListaTareas
const listaTareas = new ListaTareas();

// Añadir algunas tareas a la lista
listaTareas.añadirTarea(new Tarea("Tarea 1", "Descripción de la tarea 1", "2023-03-08", "Alta"));
listaTareas.añadirTarea(new Tarea("Tarea 2", "Descripción de la tarea 2", "2023-03-15", "Media"));
listaTareas.añadirTarea(new Tarea("Tarea 3", "Descripción de la tarea 3", "2023-03-22", "Baja"));

// Obtener todas las tareas de la lista
console.log("Todas las tareas:");
console.log(listaTareas.obtenerTareas());

// Obtener todas las tareas completadas de la lista
console.log("Tareas completadas:");
console.log(listaTareas.obtenerTareasCompletadas());

// Obtener todas las tareas pendientes de la lista
console.log("Tareas pendientes:");
console.log(listaTareas.obtenerTareasPendientes());

// Eliminar una tarea de la lista
const tareaAEliminar = listaTareas.tareas[1];
listaTareas.eliminarTarea(tareaAEliminar);

// Obtener todas las tareas de la lista después de eliminar una tarea
console.log("Todas las tareas después de eliminar una tarea:");
console.log(listaTareas.obtenerTareas());
```

Este código es un ejemplo de cómo se puede utilizar TypeScript para crear una lista de tareas. La clase `Tarea` representa una sola tarea, con propiedades para el título, la descripción, la fecha límite y la prioridad. La clase `ListaTareas` representa una lista de tareas, con métodos para añadir, obtener, eliminar y filtrar tareas.

El código crea una instancia de la clase `ListaTareas` y añade tres tareas a la lista. Luego, obtiene todas las tareas de la lista, todas las tareas completadas y todas las tareas pendientes. A continuación, elimina una tarea de la lista y vuelve a obtener todas las tareas de la lista.

El código utiliza una serie de características de TypeScript, incluyendo clases, propiedades, métodos, funciones flecha y genéricos. También utiliza el operador de propagación para copiar el contenido de una matriz en otra.