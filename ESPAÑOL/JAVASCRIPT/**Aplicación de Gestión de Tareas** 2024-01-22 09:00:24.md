```javascript
// Declaración de variables
let listaDeTareas = []; // Array para almacenar las tareas
let tareaActual = null; // Tarea actual que se está realizando
let tiempoRestante = 0; // Tiempo restante para completar la tarea actual

// Función para crear una nueva tarea
const crearTarea = (titulo, descripcion, tiempoEstimado) => {
  // Crea un objeto con la información de la tarea
  const tarea = {
    titulo: titulo,
    descripcion: descripcion,
    tiempoEstimado: tiempoEstimado,
  };

  // Agrega la tarea a la lista de tareas
  listaDeTareas.push(tarea);

  // Actualiza la tarea actual (si no hay una tarea actual, se establece como la primera de la lista)
  if (!tareaActual) {
    tareaActual = listaDeTareas[0];
  }

  // Retorna la tarea creada
  return tarea;
};

// Función para iniciar una tarea
const iniciarTarea = () => {
  // Si no hay una tarea actual, se detiene la función
  if (!tareaActual) {
    return;
  }

  // Establece el tiempo restante como el tiempo estimado de la tarea
  tiempoRestante = tareaActual.tiempoEstimado;

  // Inicia un temporizador que cuenta el tiempo restante
  const intervalo = setInterval(() => {
    // Decrementa el tiempo restante en un segundo
    tiempoRestante--;

    // Si el tiempo restante llega a cero, se detiene el temporizador
    if (tiempoRestante <= 0) {
      clearInterval(intervalo);

      // Se marca la tarea como completada
      tareaActual.completada = true;

      // Actualiza la tarea actual (si es la última tarea, se establece como la primera de la lista)
      if (tareaActual === listaDeTareas[listaDeTareas.length - 1]) {
        tareaActual = listaDeTareas[0];
      } else {
        tareaActual = listaDeTareas[listaDeTareas.indexOf(tareaActual) + 1];
      }
    }
  }, 1000);
};

// Función para detener una tarea
const detenerTarea = () => {
  // Si no hay una tarea actual, se detiene la función
  if (!tareaActual) {
    return;
  }

  // Se detiene el temporizador
  clearInterval(intervalo);

  // Actualiza el tiempo estimado de la tarea
  tareaActual.tiempoEstimado = tiempoRestante;

  // Actualiza la tarea actual (si es la última tarea, se establece como la primera de la lista)
  if (tareaActual === listaDeTareas[listaDeTareas.length - 1]) {
    tareaActual = listaDeTareas[0];
  } else {
    tareaActual = listaDeTareas[listaDeTareas.indexOf(tareaActual) + 1];
  }
};

// Función para marcar una tarea como completada
const completarTarea = () => {
  // Si no hay una tarea actual, se detiene la función
  if (!tareaActual) {
    return;
  }

  // Marca la tarea como completada
  tareaActual.completada = true;

  // Actualiza la tarea actual (si es la última tarea, se establece como la primera de la lista)
  if (tareaActual === listaDeTareas[listaDeTareas.length - 1]) {
    tareaActual = listaDeTareas[0];
  } else {
    tareaActual = listaDeTareas[listaDeTareas.indexOf(tareaActual) + 1];
  }
};

// Función para eliminar una tarea
const eliminarTarea = (tarea) => {
  // Elimina la tarea de la lista de tareas
  const index = listaDeTareas.indexOf(tarea);
  listaDeTareas.splice(index, 1);

  // Actualiza la tarea actual (si es la tarea eliminada, se establece como la primera de la lista)
  if (tarea === tareaActual) {
    tareaActual = listaDeTareas[0];
  }
};

// Función para obtener la lista de tareas
const obtenerListaDeTareas = () => {
  return listaDeTareas;
};

// Función para obtener la tarea actual
const obtenerTareaActual = () => {
  return tareaActual;
};

// Función para obtener el tiempo restante
const obtenerTiempoRestante = () => {
  return tiempoRestante;
};

// Exporta las funciones para usarlas en otros módulos
export {
