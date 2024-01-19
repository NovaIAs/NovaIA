```javascript
// Declaración de variables.
var listaDeTareas = ["Ir a la tienda", "Hacer la colada", "Limpiar la casa", "Estudiar para el examen"];
var tareaActual = "";
var indiceActual = 0;

// Función para mostrar la lista de tareas.
function mostrarListaDeTareas() {
  console.log("Lista de tareas:");
  for (var i = 0; i < listaDeTareas.length; i++) {
    console.log(`${i + 1}. ${listaDeTareas[i]}`);
  }
}

// Función para agregar una nueva tarea a la lista.
function agregarTarea(nuevaTarea) {
  listaDeTareas.push(nuevaTarea);
  mostrarListaDeTareas();
}

// Función para eliminar una tarea de la lista.
function eliminarTarea(indice) {
  listaDeTareas.splice(indice, 1);
  mostrarListaDeTareas();
}

// Función para marcar una tarea como completada.
function marcarTareaComoCompletada(indice) {
  listaDeTareas[indice] = `~${listaDeTareas[indice]}~`;
  mostrarListaDeTareas();
}

// Función para desmarcar una tarea como completada.
function desmarcarTareaComoCompletada(indice) {
  listaDeTareas[indice] = listaDeTareas[indice].slice(1, -1);
  mostrarListaDeTareas();
}

// Función para iniciar la aplicación.
function iniciarAplicacion() {
  mostrarListaDeTareas();

  // Escuchar eventos.
  document.getElementById("btnAgregarTarea").addEventListener("click", function () {
    var nuevaTarea = document.getElementById("txtNuevaTarea").value;
    agregarTarea(nuevaTarea);
  });

  document.getElementById("btnEliminarTarea").addEventListener("click", function () {
    var indice = document.getElementById("txtIndiceTareaAEliminar").value;
    eliminarTarea(indice);
  });

  document.getElementById("btnMarcarTareaComoCompletada").addEventListener("click", function () {
    var indice = document.getElementById("txtIndiceTareaAMarcarComoCompletada").value;
    marcarTareaComoCompletada(indice);
  });

  document.getElementById("btnDesmarcarTareaComoCompletada").addEventListener("click", function () {
    var indice = document.getElementById("txtIndiceTareaADesmarcarComoCompletada").value;
    desmarcarTareaComoCompletada(indice);
  });
}

// Iniciar la aplicación.
iniciarAplicacion();
```

Este código es un gestor de tareas completo. Permite al usuario agregar, eliminar y marcar tareas como completadas o desmarcadas. También muestra una lista de tareas pendientes.

El código está dividido en varias funciones, cada una de las cuales realiza una tarea específica. Por ejemplo, la función `mostrarListaDeTareas()` muestra la lista de tareas pendientes, la función `agregarTarea()` agrega una nueva tarea a la lista, y la función `eliminarTarea()` elimina una tarea de la lista.

Las funciones están organizadas de tal manera que se pueden utilizar para crear diferentes tipos de gestores de tareas. Por ejemplo, se podría crear un gestor de tareas que permita al usuario asignar prioridades a las tareas, o un gestor de tareas que permita al usuario crear subtareas.

El código también es extensible. Se pueden agregar nuevas funciones para mejorar la funcionalidad del gestor de tareas. Por ejemplo, se podría agregar una función que permita al usuario exportar la lista de tareas a un archivo de texto.

En general, este código es un buen ejemplo de cómo se puede utilizar JavaScript para crear aplicaciones complejas y funcionales.