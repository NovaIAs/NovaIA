```cool
clase Empresa {
  nombre: String;
  empleados: Array[Empleado];
  clientes: Array[Cliente];

  constructor(nombre) {
    this.nombre = nombre;
    this.empleados = [];
    this.clientes = [];
  }

  agregarEmpleado(empleado) {
    this.empleados.add(empleado);
  }

  agregarCliente(cliente) {
    this.clientes.add(cliente);
  }

  obtenerTotalEmpleados() {
    return this.empleados.length;
  }

  obtenerTotalClientes() {
    return this.clientes.length;
  }
}

clase Empleado {
  nombre: String;
  salario: Float;

  constructor(nombre, salario) {
    this.nombre = nombre;
    this.salario = salario;
  }

  obtenerNombre() {
    return this.nombre;
  }

  obtenerSalario() {
    return this.salario;
  }
}

clase Cliente {
  nombre: String;
  direccion: String;

  constructor(nombre, direccion) {
    this.nombre = nombre;
    this.direccion = direccion;
  }

  obtenerNombre() {
    return this.nombre;
  }

  obtenerDireccion() {
    return this.direccion;
  }
}

empresa = Empresa("ACME");

empleado1 = Empleado("Juan", 1000.0);
empleado2 = Empleado("María", 1200.0);

cliente1 = Cliente("Pedro", "Calle Mayor, 123");
cliente2 = Cliente("Ana", "Calle Menor, 456");

empresa.agregarEmpleado(empleado1);
empresa.agregarEmpleado(empleado2);

empresa.agregarCliente(cliente1);
empresa.agregarCliente(cliente2);

NSLog("Total de empleados: ".concat(empresa.obtenerTotalEmpleados().toString()));
NSLog("Total de clientes: ".concat(empresa.obtenerTotalClientes().toString()));
```

Este código crea una clase `Empresa` con un nombre, una lista de empleados y una lista de clientes. También define una clase `Empleado` con un nombre y un salario, y una clase `Cliente` con un nombre y una dirección.

El código crea un objeto `empresa` de tipo `Empresa` y dos objetos `empleado1` y `empleado2` de tipo `Empleado`. También crea dos objetos `cliente1` y `cliente2` de tipo `Cliente`.

Luego, el código agrega los objetos `empleado1` y `empleado2` a la lista de empleados de la empresa mediante el método `agregarEmpleado`. También agrega los objetos `cliente1` y `cliente2` a la lista de clientes de la empresa mediante el método `agregarCliente`.

Finalmente, el código imprime el número total de empleados y clientes en la consola mediante los métodos `obtenerTotalEmpleados` y `obtenerTotalClientes` de la clase `Empresa`.

Este código es complejo y contiene varias clases y objetos, pero es un buen ejemplo de cómo usar la programación orientada a objetos en Cool.