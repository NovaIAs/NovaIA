```typescript
// Definimos una interfaz para representar una persona.
interface Persona {
  nombre: string;
  apellido: string;
  edad: number;
}

// Creamos una clase para representar un empleado.
class Empleado implements Persona {
  nombre: string;
  apellido: string;
  edad: number;
  cargo: string;
  salario: number;

  constructor(nombre: string, apellido: string, edad: number, cargo: string, salario: number) {
    this.nombre = nombre;
    this.apellido = apellido;
    this.edad = edad;
    this.cargo = cargo;
    this.salario = salario;
  }

  // Método para obtener el nombre completo del empleado.
  getNombreCompleto(): string {
    return `${this.nombre} ${this.apellido}`;
  }

  // Método para obtener el salario anual del empleado.
  getSalarioAnual(): number {
    return this.salario * 12;
  }
}

// Creamos una clase para representar una empresa.
class Empresa {
  nombre: string;
  direccion: string;
  empleados: Empleado[];

  constructor(nombre: string, direccion: string) {
    this.nombre = nombre;
    this.direccion = direccion;
    this.empleados = [];
  }

  // Método para agregar un nuevo empleado a la empresa.
  agregarEmpleado(empleado: Empleado): void {
    this.empleados.push(empleado);
  }

  // Método para obtener el número total de empleados de la empresa.
  getNumEmpleados(): number {
    return this.empleados.length;
  }

  // Método para obtener la nómina total de la empresa.
  getNominaTotal(): number {
    let nominaTotal = 0;
    for (let i = 0; i < this.empleados.length; i++) {
      nominaTotal += this.empleados[i].getSalarioAnual();
    }
    return nominaTotal;
  }
}

// Creamos una empresa llamada "Acme Corporation".
const empresa = new Empresa("Acme Corporation", "123 Main Street");

// Creamos algunos empleados y los agregamos a la empresa.
const empleado1 = new Empleado("Juan", "Pérez", 25, "Gerente", 10000);
const empleado2 = new Empleado("María", "López", 30, "Ingeniera", 8000);
const empleado3 = new Empleado("Pedro", "García", 35, "Contador", 6000);

empresa.agregarEmpleado(empleado1);
empresa.agregarEmpleado(empleado2);
empresa.agregarEmpleado(empleado3);

// Mostramos el nombre completo de los empleados.
console.log("Empleados:");
for (let i = 0; i < empresa.empleados.length; i++) {
  console.log(empresa.empleados[i].getNombreCompleto());
}

// Mostramos el número total de empleados de la empresa.
console.log("Número total de empleados:", empresa.getNumEmpleados());

// Mostramos la nómina total de la empresa.
console.log("Nómina total:", empresa.getNominaTotal());
```

Este código es un ejemplo de un programa complejo en TypeScript. Define una interfaz para representar una persona, una clase para representar un empleado, una clase para representar una empresa, y crea una instancia de cada uno. Luego, el programa muestra el nombre completo de los empleados, el número total de empleados de la empresa y la nómina total de la empresa.

Este código es un ejemplo de cómo se puede usar TypeScript para crear programas complejos y estructurados. TypeScript es un lenguaje fuertemente tipado que permite detectar errores en tiempo de compilación, lo que lo convierte en una buena opción para desarrollar programas grandes y complejos.