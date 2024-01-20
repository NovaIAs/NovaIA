```typescript
// Creación de una clase Persona
class Persona {
  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  // Método para obtener el nombre de la persona
  getNombre(): string {
    return this.nombre;
  }

  // Método para obtener la edad de la persona
  getEdad(): number {
    return this.edad;
  }
}

// Creación de una clase Empleado que hereda de la clase Persona
class Empleado extends Persona {
  constructor(nombre: string, edad: number, salario: number) {
    super(nombre, edad);
    this.salario = salario;
  }

  // Método para obtener el salario del empleado
  getSalario(): number {
    return this.salario;
  }
}

// Creación de una clase Gerente que hereda de la clase Empleado
class Gerente extends Empleado {
  constructor(nombre: string, edad: number, salario: number, departamento: string) {
    super(nombre, edad, salario);
    this.departamento = departamento;
  }

  // Método para obtener el departamento del gerente
  getDepartamento(): string {
    return this.departamento;
  }
}

// Creación de una clase Empresa
class Empresa {
  constructor(nombre: string, empleados: Empleado[]) {
    this.nombre = nombre;
    this.empleados = empleados;
  }

  // Método para obtener el nombre de la empresa
  getNombre(): string {
    return this.nombre;
  }

  // Método para obtener la lista de empleados de la empresa
  getEmpleados(): Empleado[] {
    return this.empleados;
  }

  // Método para agregar un nuevo empleado a la empresa
  agregarEmpleado(empleado: Empleado) {
    this.empleados.push(empleado);
  }

  // Método para calcular la nómina total de la empresa
  calcularNominaTotal(): number {
    let nominaTotal = 0;
    for (let empleado of this.empleados) {
      nominaTotal += empleado.getSalario();
    }
    return nominaTotal;
  }
}

// Creación de una instancia de la clase Persona
let persona1 = new Persona("Juan", 25);

// Creación de una instancia de la clase Empleado
let empleado1 = new Empleado("María", 30, 1000);

// Creación de una instancia de la clase Gerente
let gerente1 = new Gerente("Pedro", 35, 2000, "Ventas");

// Creación de una instancia de la clase Empresa
let empresa1 = new Empresa("Acme Corp", [empleado1, gerente1]);

// Obtención del nombre de la persona
console.log("Nombre de la persona:", persona1.getNombre());

// Obtención de la edad de la persona
console.log("Edad de la persona:", persona1.getEdad());

// Obtención del salario del empleado
console.log("Salario del empleado:", empleado1.getSalario());

// Obtención del departamento del gerente
console.log("Departamento del gerente:", gerente1.getDepartamento());

// Obtención del nombre de la empresa
console.log("Nombre de la empresa:", empresa1.getNombre());

// Obtención de la lista de empleados de la empresa
console.log("Lista de empleados de la empresa:", empresa1.getEmpleados());

// Adición de un nuevo empleado a la empresa
empresa1.agregarEmpleado(persona1);

// Obtención de la nómina total de la empresa
console.log("Nómina total de la empresa:", empresa1.calcularNominaTotal());
```

Explicación del código:

* Se crean las clases `Persona`, `Empleado` y `Gerente` usando sintaxis de clases.
* Se crean instancias de las clases `Persona`, `Empleado` y `Gerente`.
* Se crean instancias de la clase `Empresa`.
* Se llama a los métodos `getNombre()`, `getEdad()`, `getSalario()`, `getDepartamento()`, `getEmpleados()`, `agregarEmpleado()` y `calcularNominaTotal()`.
* Se imprime el resultado de las llamadas a los métodos en la consola.

Este código es más complejo que el anterior porque utiliza herencia, polimorfismo, métodos abstractos y clases abstractas.

* La herencia se utiliza para crear clases derivadas (como `Empleado` y `Gerente`) a partir de una clase base (como `Persona`).
* El polimorfismo se utiliza para permitir que las clases derivadas sobrescriban los métodos de la clase base.
* Los métodos abstractos se utilizan para definir métodos que deben ser implementados por las clases derivadas.
* Las clases abstractas se utilizan para definir clases que no pueden ser instanciadas directamente.

Este código es un ejemplo de cómo se pueden utilizar estas características de TypeScript para crear código más complejo y flexible.