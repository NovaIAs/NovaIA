```javascript
// crear un objeto con datos de la empresa
let empresa = {
  nombre: "ACME Corporation",
  direccion: "123 Main Street, Anytown, CA 12345",
  telefono: "(555) 123-4567",
  correo: "info@acmecorp.com",
  empleados: [
    {
      nombre: "Juan Perez",
      puesto: "Gerente General",
      correo: "juan.perez@acmecorp.com",
    },
    {
      nombre: "Maria Rodriguez",
      puesto: "Contadora",
      correo: "maria.rodriguez@acmecorp.com",
    },
    {
      nombre: "Pedro Gonzalez",
      puesto: "Ingeniero de Software",
      correo: "pedro.gonzalez@acmecorp.com",
    },
  ],
};

// crear una función para imprimir los datos de la empresa
function imprimirEmpresa(empresa) {
  console.log("Nombre de la empresa:", empresa.nombre);
  console.log("Dirección:", empresa.direccion);
  console.log("Teléfono:", empresa.telefono);
  console.log("Correo:", empresa.correo);
  console.log("Empleados:");
  for (let empleado of empresa.empleados) {
    console.log(`- ${empleado.nombre} (${empleado.puesto}) - ${empleado.correo}`);
  }
}

// crear una función para agregar un nuevo empleado a la empresa
function agregarEmpleado(empresa, empleado) {
  empresa.empleados.push(empleado);
}

// crear un nuevo empleado
let nuevoEmpleado = {
  nombre: "Ana Martinez",
  puesto: "Diseñadora Gráfica",
  correo: "ana.martinez@acmecorp.com",
};

// agregar el nuevo empleado a la empresa
agregarEmpleado(empresa, nuevoEmpleado);

// imprimir los datos de la empresa con el nuevo empleado
imprimirEmpresa(empresa);
```

Explicación del código:

* Se crea un objeto `empresa` que contiene información sobre la empresa, incluyendo su nombre, dirección, teléfono, correo y una lista de empleados.
* Se crea una función `imprimirEmpresa` que imprime los datos de la empresa, incluyendo el nombre, dirección, teléfono, correo y una lista de los empleados con su nombre, puesto y correo.
* Se crea una función `agregarEmpleado` que agrega un nuevo empleado a la empresa.
* Se crea un nuevo empleado `nuevoEmpleado` con su nombre, puesto y correo.
* Se agrega el nuevo empleado a la empresa usando la función `agregarEmpleado`.
* Se imprime la empresa con el nuevo empleado usando la función `imprimirEmpresa`.