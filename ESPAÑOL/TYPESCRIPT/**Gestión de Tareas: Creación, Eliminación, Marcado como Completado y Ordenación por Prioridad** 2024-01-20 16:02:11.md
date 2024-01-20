```typescript
// Definir una interfaz para representar una tarea
interface Tarea {
  id: number;
  nombre: string;
  descripcion: string;
  prioridad: "Alta" | "Media" | "Baja";
  estado: "Pendiente" | "En progreso" | "Completada";
}

// Definir un tipo para representar una lista de tareas
type ListaTareas = Tarea[];

// Crear una función para añadir una tarea a una lista
const añadirTarea = (listaTareas: ListaTareas, tarea: Tarea): ListaTareas => {
  return [...listaTareas, tarea];
};

// Crear una función para eliminar una tarea de una lista
const eliminarTarea = (listaTareas: ListaTareas, id: number): ListaTareas => {
  return listaTareas.filter((tarea) => tarea.id !== id);
};

// Crear una función para marcar una tarea como completada
const completarTarea = (listaTareas: ListaTareas, id: number): ListaTareas => {
  return listaTareas.map((tarea) => {
    if (tarea.id === id) {
      tarea.estado = "Completada";
    }
    return tarea;
  });
};

// Crear una función para obtener la prioridad más alta de una lista de tareas
const obtenerPrioridadMasAlta = (listaTareas: ListaTareas): "Alta" | "Media" | "Baja" => {
  const prioridades = listaTareas.map((tarea) => tarea.prioridad);
  return prioridades.reduce((a, b) => {
    if (a === "Alta") {
      return a;
    }
    if (b === "Alta") {
      return b;
    }
    if (a === "Media" && b === "Baja") {
      return a;
    }
    return b;
  });
};

// Crear una función para obtener la tarea con la prioridad más alta de una lista de tareas
const obtenerTareaConPrioridadMasAlta = (listaTareas: ListaTareas): Tarea | undefined => {
  const prioridades = listaTareas.map((tarea) => tarea.prioridad);
  const prioridadMasAlta = prioridades.reduce((a, b) => {
    if (a === "Alta") {
      return a;
    }
    if (b === "Alta") {
      return b;
    }
    if (a === "Media" && b === "Baja") {
      return a;
    }
    return b;
  });
  return listaTareas.find((tarea) => tarea.prioridad === prioridadMasAlta);
};

// Crear una función para obtener la lista de tareas ordenadas por prioridad
const ordenarTareasPorPrioridad = (listaTareas: ListaTareas): ListaTareas => {
  return listaTareas.sort((a, b) => {
    if (a.prioridad === "Alta") {
      return -1;
    }
    if (b.prioridad === "Alta") {
      return 1;
    }
    if (a.prioridad === "Media" && b.prioridad === "Baja") {
      return -1;
    }
    return 1;
  });
};
```

Explicación del código:

1. **Interfaz `Tarea`:** Define una interfaz para representar una tarea, incluyendo su identificación, nombre, descripción, prioridad y estado.
2. **`ListaTareas`:** Define un tipo para representar una lista de tareas.
3. **`añadirTarea`:** Crea una función para añadir una tarea a una lista de tareas.
4. **`eliminarTarea`:** Crea una función para eliminar una tarea de una lista de tareas.
5. **`completarTarea`:** Crea una función para marcar una tarea como completada.
6. **`obtenerPrioridadMasAlta`:** Crea una función para obtener la prioridad más alta de una lista de tareas.
7. **`obtenerTareaConPrioridadMasAlta`:** Crea una función para obtener la tarea con la prioridad más alta de una lista de tareas.
8. **`ordenarTareasPorPrioridad`:** Crea una función para ordenar las tareas de una lista por prioridad.

Este código proporciona una implementación completa para gestionar una lista de tareas, incluyendo la adición, eliminación, marcado como completado, obtención de la prioridad más alta y ordenación de las tareas por prioridad.