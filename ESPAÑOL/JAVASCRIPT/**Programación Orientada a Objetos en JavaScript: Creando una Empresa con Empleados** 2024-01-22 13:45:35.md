```javascript
// Definición de la clase Persona
class Persona {
  constructor(nombre, apellido, edad, direccion) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.edad = edad;
    this.direccion = direccion;
  }

  // Método para obtener el nombre completo de la persona
  getNombreCompleto() {
    return `${this.nombre} ${this.apellido}`;
  }

  // Método para obtener la edad de la persona
  getEdad() {
    return this.edad;
  }

  // Método para obtener la dirección de la persona
  getDireccion() {
    return this.direccion;
  }

  // Método para imprimir la información de la persona
  imprimirInformacion() {
    console.log(`Nombre: ${this.nombre}`);
    console.log(`Apellido: ${this.apellido}`);
    console.log(`Edad: ${this.edad}`);
    console.log(`Dirección: ${this.direccion}`);
  }
}

// Definición de la clase Empleado
class Empleado extends Persona {
  constructor(nombre, apellido, edad, direccion, cargo, salario) {
    super(nombre, apellido, edad, direccion);
    this.cargo = cargo;
    this.salario = salario;
  }

  // Método para obtener el cargo del empleado
  getCargo() {
    return this.cargo;
  }

  // Método para obtener el salario del empleado
  getSalario() {
    return this.salario;
  }

  // Método para imprimir la información del empleado
  imprimirInformacion() {
    super.imprimirInformacion();
    console.log(`Cargo: ${this.cargo}`);
    console.log(`Salario: ${this.salario}`);
  }
}

// Definición de la clase Empresa
class Empresa {
  constructor(nombre, direccion, empleados) {
    this.nombre = nombre;
    this.direccion = direccion;
    this.empleados = empleados;
  }

  // Método para obtener el nombre de la empresa
  getNombre() {
    return this.nombre;
  }

  // Método para obtener la dirección de la empresa
  getDireccion() {
    return this.direccion;
  }

  // Método para obtener la lista de empleados de la empresa
  getEmpleados() {
    return this.empleados;
  }

  // Método para imprimir la información de la empresa
  imprimirInformacion() {
    console.log(`Nombre: ${this.nombre}`);
    console.log(`Dirección: ${this.direccion}`);
    console.log(`Empleados:`);
    this.empleados.forEach((empleado) => {
      empleado.imprimirInformacion();
    });
  }
}

// Creación de un objeto Persona
const persona1 = new Persona("Juan", "García", 25, "Calle 123");

// Creación de un objeto Empleado
const empleado1 = new Empleado("María", "Pérez", 30, "Calle 456", "Ingeniera de Software", 100000);

// Creación de un objeto Empresa
const empresa1 = new Empresa("Acme Corporation", "Calle 789", [empleado1]);

// Impresión de la información de la empresa
empresa1.imprimirInformacion();
```

Explicación del código:

1. Definición de la clase `Persona`: Esta clase define las propiedades y métodos comunes a todas las personas.
2. Definición de la clase `Empleado`: Esta clase hereda de la clase `Persona` y define las propiedades y métodos específicos de los empleados.
3. Definición de la clase `Empresa`: Esta clase agrupa a los empleados y define las propiedades y métodos comunes a todas las empresas.
4. Creación de un objeto `Persona`: Se crea un objeto de tipo `Persona` utilizando el constructor de la clase.
5. Creación de un objeto `Empleado`: Se crea un objeto de tipo `Empleado` utilizando el constructor de la clase.
6. Creación de un objeto `Empresa`: Se crea un objeto de tipo `Empresa` utilizando el constructor de la clase.
7. Impresión de la información de la empresa: Se llama al método `imprimirInformacion()` del objeto `Empresa` para imprimir la información de la empresa.