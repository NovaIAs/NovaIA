```javascript
// Función principal que se ejecuta al cargar la página
function main() {
  // Obtiene el elemento DOM con el ID "tabla"
  const tabla = document.getElementById("tabla");

  // Crea un nuevo encabezado para la tabla
  const encabezado = document.createElement("thead");

  // Crea una nueva fila para el encabezado
  const filaEncabezado = document.createElement("tr");

  // Crea las celdas para el encabezado
  const celdaNombre = document.createElement("th");
  const celdaApellido = document.createElement("th");
  const celdaEdad = document.createElement("th");

  // Agrega texto a las celdas del encabezado
  celdaNombre.textContent = "Nombre";
  celdaApellido.textContent = "Apellido";
  celdaEdad.textContent = "Edad";

  // Agrega las celdas del encabezado a la fila del encabezado
  filaEncabezado.appendChild(celdaNombre);
  filaEncabezado.appendChild(celdaApellido);
  filaEncabezado.appendChild(celdaEdad);

  // Agrega la fila del encabezado al encabezado de la tabla
  encabezado.appendChild(filaEncabezado);

  // Agrega el encabezado de la tabla a la tabla
  tabla.appendChild(encabezado);

  // Crea un nuevo cuerpo para la tabla
  const cuerpo = document.createElement("tbody");

  // Crea una nueva fila para el cuerpo
  const filaCuerpo = document.createElement("tr");

  // Crea las celdas para el cuerpo
  const celdaNombre2 = document.createElement("td");
  const celdaApellido2 = document.createElement("td");
  const celdaEdad2 = document.createElement("td");

  // Agrega texto a las celdas del cuerpo
  celdaNombre2.textContent = "Juan";
  celdaApellido2.textContent = "Pérez";
  celdaEdad2.textContent = "25";

  // Agrega las celdas del cuerpo a la fila del cuerpo
  filaCuerpo.appendChild(celdaNombre2);
  filaCuerpo.appendChild(celdaApellido2);
  filaCuerpo.appendChild(celdaEdad2);

  // Agrega la fila del cuerpo al cuerpo de la tabla
  cuerpo.appendChild(filaCuerpo);

  // Agrega el cuerpo de la tabla a la tabla
  tabla.appendChild(cuerpo);
}

// Se llama a la función main al cargar la página
window.onload = main;
```

Explicación del código:

* La función `main()` se ejecuta cuando la página se carga.
* Obtiene el elemento DOM con el ID "tabla" y crea un nuevo encabezado y cuerpo para la tabla.
* Crea las celdas para el encabezado y el cuerpo de la tabla, y les agrega texto.
* Agrega las celdas a las filas, y las filas al encabezado y al cuerpo de la tabla.
* Finalmente, agrega el encabezado y el cuerpo de la tabla a la tabla.

Este código crea una tabla HTML con un encabezado y un cuerpo. El encabezado contiene las celdas "Nombre", "Apellido" y "Edad", y el cuerpo contiene una fila con los valores "Juan", "Pérez" y "25".