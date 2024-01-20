```typescript
// 1. Definición de interfaces
interface Persona {
  nombre: string;
  edad: number;
  ocupacion: string;
}

interface Direccion {
  calle: string;
  ciudad: string;
  estado: string;
  codigoPostal: number;
}

// 2. Definición de clases
class Empleado implements Persona {
  nombre: string;
  edad: number;
  ocupacion: string;

  constructor(nombre: string, edad: number, ocupacion: string) {
    this.nombre = nombre;
    this.edad = edad;
    this.ocupacion = ocupacion;
  }
}

class Ingeniero extends Empleado {
  especialidad: string;

  constructor(nombre: string, edad: number, especialidad: string) {
    super(nombre, edad, "Ingeniero");
    this.especialidad = especialidad;
  }
}

// 3. Definición de funciones
function calcularSalario(empleado: Empleado): number {
  // Lógica para calcular el salario en función del tipo de empleado

  return 1000; // Ejemplo de salario fijo por simplicidad
}

function imprimirDatos(persona: Persona, direccion: Direccion): void {
  console.log(`Nombre: ${persona.nombre}`);
  console.log(`Edad: ${persona.edad}`);
  console.log(`Ocupación: ${persona.ocupacion}`);
  console.log(`Dirección: ${direccion.calle}, ${direccion.ciudad}, ${direccion.estado}, ${direccion.codigoPostal}`);
}

// 4. Creación de objetos
const empleado1 = new Ingeniero("Juan Pérez", 30, "Software");
const direccion1 = {
  calle: "Calle Mayor",
  ciudad: "Madrid",
  estado: "España",
  codigoPostal: 28001
};

// 5. Uso de las funciones
const salario = calcularSalario(empleado1);
imprimirDatos(empleado1, direccion1);

console.log(`Salario: ${salario}`);
```

Explicación:

1. **Definición de interfaces:** Se definen dos interfaces, `Persona` y `Direccion`, que especifican las propiedades que deben tener los objetos que implementen dichas interfaces.

2. **Definición de clases:** Se definen dos clases, `Empleado` e `Ingeniero`, que implementan la interfaz `Persona`. La clase `Ingeniero` hereda de la clase `Empleado` y añade una propiedad adicional, `especialidad`.

3. **Definición de funciones:** Se definen dos funciones, `calcularSalario` e `imprimirDatos`. La función `calcularSalario` calcula el salario de un empleado en función del tipo de empleado. La función `imprimirDatos` imprime los datos de una persona y su dirección.

4. **Creación de objetos:** Se crean dos objetos, `empleado1` y `direccion1`, utilizando las clases e interfaces definidas anteriormente.

5. **Uso de las funciones:** Se llama a las funciones `calcularSalario` e `imprimirDatos` pasándoles los objetos creados anteriormente.

Este código es más complejo que el anterior, ya que involucra la definición y uso de interfaces, clases, funciones y herencia.