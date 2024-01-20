```typescript
// Se define una interfaz para representar un objeto de tipo "Tarea".
interface Tarea {
  id: number;
  titulo: string;
  descripcion: string;
  estado: string; // Puede ser "pendiente", "en progreso" o "terminado".
}

// Se crea una clase "GestorDeTareas" que tendrá métodos para gestionar tareas.
class GestorDeTareas {
  // Se definen las propiedades de la clase.
  private tareas: Tarea[] = []; // Almacena todas las tareas.
  private idContador: number = 0; // Se utiliza para generar identificadores únicos para las tareas.

  // Método para añadir una nueva tarea.
  public añadirTarea(titulo: string, descripcion: string): number {
    // Se genera un nuevo identificador único para la tarea.
    const id = ++this.idContador;

    // Se crea un nuevo objeto de tipo "Tarea" con los datos proporcionados.
    const tarea: Tarea = {
      id: id,
      titulo: titulo,
      descripcion: descripcion,
      estado: "pendiente",
    };

    // Se añade la tarea al array de tareas.
    this.tareas.push(tarea);

    // Se devuelve el identificador de la tarea recién creada.
    return id;
  }

  // Método para obtener todas las tareas.
  public obtenerTareas(): Tarea[] {
    return this.tareas;
  }

  // Método para obtener una tarea por su identificador.
  public obtenerTareaPorId(id: number): Tarea | undefined {
    // Se busca la tarea en el array de tareas utilizando el identificador proporcionado.
    const tarea = this.tareas.find((tarea) => tarea.id === id);

    // Se devuelve la tarea o `undefined` si no se encuentra.
    return tarea;
  }

  // Método para actualizar una tarea.
  public actualizarTarea(id: number, titulo: string, descripcion: string): void {
    // Se busca la tarea en el array de tareas utilizando el identificador proporcionado.
    const tarea = this.tareas.find((tarea) => tarea.id === id);

    // Si se encuentra la tarea, se actualizan sus datos.
    if (tarea) {
      tarea.titulo = titulo;
      tarea.descripcion = descripcion;
    }
  }

  // Método para eliminar una tarea.
  public eliminarTarea(id: number): void {
    // Se busca la tarea en el array de tareas utilizando el identificador proporcionado.
    const tareaIndex = this.tareas.findIndex((tarea) => tarea.id === id);

    // Si se encuentra la tarea, se elimina del array.
    if (tareaIndex !== -1) {
      this.tareas.splice(tareaIndex, 1);
    }
  }

  // Método para cambiar el estado de una tarea.
  public cambiarEstadoTarea(id: number, nuevoEstado: string): void {
    // Se busca la tarea en el array de tareas utilizando el identificador proporcionado.
    const tarea = this.tareas.find((tarea) => tarea.id === id);

    // Si se encuentra la tarea, se actualiza su estado.
    if (tarea) {
      tarea.estado = nuevoEstado;
    }
  }
}

// Se crea una instancia de la clase "GestorDeTareas".
const gestorDeTareas = new GestorDeTareas();

// Se añaden algunas tareas de ejemplo.
gestorDeTareas.añadirTarea("Comprar comida", "Comprar alimentos frescos y saludables para la semana.");
gestorDeTareas.añadirTarea("Hacer ejercicio", "Correr 30 minutos en el parque.");
gestorDeTareas.añadirTarea("Estudiar para el examen", "Repasar los apuntes y hacer ejercicios de práctica.");

// Se obtienen todas las tareas.
const tareas = gestorDeTareas.obtenerTareas();

// Se muestran las tareas en la consola.
console.log("Todas las tareas:");
tareas.forEach((tarea) => {
  console.log(`- ${tarea.titulo}: ${tarea.descripcion} (Estado: ${tarea.estado})`);
});

// Se obtiene una tarea por su identificador.
const tareaPorId = gestorDeTareas.obtenerTareaPorId(2);

// Si se encontró la tarea, se muestra su información.
if (tareaPorId) {
  console.log("\nTarea con ID 2:");
  console.log(`- Título: ${tareaPorId.titulo}`);
  console.log(`- Descripción: ${tareaPorId.descripcion}`);
  console.log(`- Estado: ${tareaPorId.estado}`);
}

// Se actualiza una tarea.
gestorDeTareas.actualizarTarea(1, "Comprar alimentos frescos", "Comprar frutas, verduras, carne y pescado frescos para la semana.");

// Se elimina una tarea.
gestorDeTareas.eliminarTarea(3);

// Se cambia el estado de una tarea.
gestorDeTareas.cambiarEstadoTarea(2, "terminado");

// Se vuelven a obtener todas las tareas.
const tareasActualizadas = gestorDeTareas.obtenerTareas();

// Se muestran las tareas actualizadas en la consola.
console.log("\nTodas las tareas actualizadas:");
tareasActualizadas.forEach((tarea) => {
  console.log(`- ${tarea.titulo}: ${tarea.descripcion} (Estado: ${tarea.estado})`);
});
```

Explicación del código:

1. Se define una interfaz `Tarea` que representa un objeto con las propiedades `id`, `titulo`, `descripcion` y `estado`.

2. Se crea una clase `GestorDeTareas` que tiene métodos para gestionar tareas, como añadir, obtener, actualizar, eliminar y cambiar el estado de las tareas.

3. Se crea una instancia de la clase `GestorDeTareas` llamada `gestorDeTareas`.

4. Se añaden algunas tareas de ejemplo al gestor de tareas.

5. Se obtienen todas las tareas y se muestran en la consola.

6. Se obtiene una tarea por su identificador y se muestra su información.

7. Se actualiza una tarea y se muestra la tarea actualizada.

8. Se elimina una tarea y se muestran las tareas restantes.

9. Se cambia el estado de una tarea y se muestran las tareas actualizadas.

10. Se vuelven a obtener todas las tareas y se muestran en la consola.