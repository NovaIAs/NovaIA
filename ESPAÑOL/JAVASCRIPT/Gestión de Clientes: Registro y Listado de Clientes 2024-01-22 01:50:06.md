```javascript
// Programa: Gestión de clientes - Inicio

// Declaración de los objetos necesarios.
const clientes = []; // Array para almacenar los clientes.
const formulario = document.getElementById("formulario"); // Formulario de ingreso de datos.
const nombre = document.getElementById("nombre"); // Campo de texto para el nombre.
const apellido = document.getElementById("apellido"); // Campo de texto para el apellido.
const email = document.getElementById("email"); // Campo de texto para el email.
const telefono = document.getElementById("telefono"); // Campo de texto para el teléfono.
const direccion = document.getElementById("direccion"); // Campo de texto para la dirección.

// Función para agregar un nuevo cliente.
function agregarCliente(e) {
  e.preventDefault(); // Para evitar que el formulario se envíe y recargue la página.

  // Validación de los campos de entrada.
  if (nombre.value === "" || apellido.value === "" || email.value === "" || telefono.value === "" || direccion.value === "") {
    alert("Por favor, complete todos los campos.");
    return; // Si hay algún campo vacío, finaliza la función.
  }

  // Creación del objeto cliente.
  const cliente = {
    nombre: nombre.value,
    apellido: apellido.value,
    email: email.value,
    telefono: telefono.value,
    direccion: direccion.value,
  };

  // Añadir el cliente al array de clientes.
  clientes.push(cliente);

  // Mostrar un mensaje de confirmación.
  alert("Cliente agregado con éxito.");

  // Restablecer los campos del formulario.
  nombre.value = "";
  apellido.value = "";
  email.value = "";
  telefono.value = "";
  direccion.value = "";
}

// Función para mostrar la lista de clientes.
function mostrarClientes() {
  let listadoClientes = "<h2>Listado de clientes</h2><ul>";

  for (let cliente of clientes) {
    listadoClientes += `<li>${cliente.nombre} ${cliente.apellido} - ${cliente.email} - ${cliente.telefono} - ${cliente.direccion}</li>`;
  }

  listadoClientes += "</ul>";

  document.getElementById("listado").innerHTML = listadoClientes;
}

// Asignación de los eventos.
formulario.addEventListener("submit", agregarCliente);
document.getElementById("mostrar_clientes").addEventListener("click", mostrarClientes);

// Programa: Gestión de clientes - Fin
```

Explicación del código:

1. Declaración de variables y constantes:

- **clientes:** Array para almacenar los objetos cliente.
- **formulario:** Elemento HTML del formulario.
- **nombre, apellido, email, telefono, direccion:** Elementos HTML de los campos de texto.

2. Función **agregarCliente:**

- Se encarga de agregar un nuevo cliente al array de clientes.
- Valida si todos los campos están completos y muestra un mensaje de error si alguno está vacío.
- Crea un objeto cliente con los datos ingresados.
- Añade el cliente al array de clientes.
- Muestra un mensaje de confirmación.
- Restablece los campos del formulario.

3. Función **mostrarClientes:**

- Genera una lista de los clientes almacenados en el array y la muestra en la página HTML.

4. Asignación de eventos:

- Al formulario se le asigna el evento "submit" y cuando se envía, llama a la función **agregarCliente**.
- Al botón "mostrar_clientes" se le asigna el evento "click" y cuando se hace clic, llama a la función **mostrarClientes**.