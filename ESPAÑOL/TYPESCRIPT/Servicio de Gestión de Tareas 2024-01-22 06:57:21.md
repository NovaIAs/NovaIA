```typescript
// Definir una interfaz para representar una tarea.
interface Tarea {
  id: number;
  titulo: string;
  descripcion: string;
  estado: string;
  fechaCreacion: Date;
  fechaVencimiento: Date;
  prioridad: string;
}

// Definir una clase para representar un servicio de tareas.
class ServicioTareas {

  // Constructor para inicializar el servicio con una lista de tareas.
  constructor(private tareas: Tarea[]) {}

  // Método para obtener todas las tareas.
  obtenerTodasLasTareas(): Tarea[] {
    return this.tareas;
  }

  // Método para obtener una tarea por su ID.
  obtenerTareaPorId(id: number): Tarea | undefined {
    return this.tareas.find((tarea) => tarea.id === id);
  }

  // Método para crear una nueva tarea.
  crearTarea(tarea: Tarea): void {
    this.tareas.push(tarea);
  }

  // Método para actualizar una tarea existente.
  actualizarTarea(tarea: Tarea): void {
    const tareaExistente = this.obtenerTareaPorId(tarea.id);
    if (tareaExistente) {
      tareaExistente.titulo = tarea.titulo;
      tareaExistente.descripcion = tarea.descripcion;
      tareaExistente.estado = tarea.estado;
      tareaExistente.fechaVencimiento = tarea.fechaVencimiento;
      tareaExistente.prioridad = tarea.prioridad;
    }
  }

  // Método para eliminar una tarea existente.
  eliminarTarea(id: number): void {
    const tareaIndex = this.tareas.findIndex((tarea) => tarea.id === id);
    if (tareaIndex > -1) {
      this.tareas.splice(tareaIndex, 1);
    }
  }
}

// Crear una instancia del servicio de tareas.
const servicioTareas = new ServicioTareas([
  {
    id: 1,
    titulo: 'Comprar leche',
    descripcion: 'Necesito comprar leche para el desayuno.',
    estado: 'Pendiente',
    fechaCreacion: new Date(),
    fechaVencimiento: new Date(),
    prioridad: 'Baja',
  },
  {
    id: 2,
    titulo: 'Hacer la cama',
    descripcion: 'Necesito hacer la cama antes de salir de casa.',
    estado: 'Completada',
    fechaCreacion: new Date(),
    fechaVencimiento: new Date(),
    prioridad: 'Media',
  },
  {
    id: 3,
    titulo: 'Estudiar para el examen',
    descripcion: 'Necesito estudiar para el examen de mañana.',
    estado: 'En progreso',
    fechaCreacion: new Date(),
    fechaVencimiento: new Date(),
    prioridad: 'Alta',
  },
]);

// Obtener todas las tareas.
const tareas = servicioTareas.obtenerTodasLasTareas();

// Obtener una tarea por su ID.
const tarea = servicioTareas.obtenerTareaPorId(2);

// Crear una nueva tarea.
const nuevaTarea: Tarea = {
  id: 4,
  titulo: 'Ir al gimnasio',
  descripcion: 'Necesito ir al gimnasio para hacer ejercicio.',
  estado: 'Pendiente',
  fechaCreacion: new Date(),
  fechaVencimiento: new Date(),
  prioridad: 'Media',
};
servicioTareas.crearTarea(nuevaTarea);

// Actualizar una tarea existente.
const tareaActualizada: Tarea = {
  id: 3,
  titulo: 'Estudiar para el examen de matemáticas',
  descripcion: 'Necesito estudiar para el examen de matemáticas de mañana.',
  estado: 'En progreso',
  fechaCreacion: new Date(),
  fechaVencimiento: new Date(),
  prioridad: 'Alta',
};
servicioTareas.actualizarTarea(tareaActualizada);

// Eliminar una tarea existente.
servicioTareas.eliminarTarea(1);

// Obtener todas las tareas nuevamente.
const tareasActualizadas = servicioTareas.obtenerTodasLasTareas();
```

Explicación del código:

* La interfaz `Tarea` define la estructura de una tarea.
* La clase `ServicioTareas` representa un servicio que proporciona métodos para gestionar tareas.
* El constructor del servicio de tareas se utiliza para inicializar el servicio con una lista de tareas.
* Los métodos del servicio de tareas permiten obtener todas las tareas, obtener una tarea por su ID, crear una nueva tarea, actualizar una tarea existente y eliminar una tarea existente.
* La instancia `servicioTareas` se crea con una lista de tareas de ejemplo.
* Las tareas se obtienen utilizando el método `obtenerTodasLasTareas()`.
* Una tarea se obtiene por su ID utilizando el método `obtenerTareaPorId()`.
* Se crea una nueva tarea utilizando el método `crearTarea()`.
* Una tarea existente se actualiza utilizando el método `actualizarTarea()`.
* Una tarea existente se elimina utilizando el método `eliminarTarea()`.
* Las tareas se obtienen nuevamente utilizando el método `obtenerTodasLasTareas()` para mostrar las tareas actualizadas.