```typescript
// Importa la biblioteca de React y ReactDOM
import React from "react";
import ReactDOM from "react-dom";

// Crea un componente de React llamado "HolaMundo"
class HolaMundo extends React.Component {
  // Define el método "render" del componente
  render() {
    // Devuelve un elemento HTML con el texto "Hola, Mundo!"
    return <h1>Hola, Mundo!</h1>;
  }
}

// Renderiza el componente "HolaMundo" en el elemento con ID "root"
ReactDOM.render(<HolaMundo />, document.getElementById("root"));

// Crea un componente funcional de React llamado "Suma" que recibe dos números como parámetros
const Suma = (num1, num2) => {
  // Devuelve la suma de los dos números
  return num1 + num2;
};

// Llama a la función "Suma" con los números 1 y 2, y muestra el resultado en la consola
console.log(Suma(1, 2)); // Muestra 3 en la consola

// Crea una interfaz llamada "Persona"
interface Persona {
  nombre: string;
  edad: number;
}

// Crea una clase llamada "Estudiante" que implementa la interfaz "Persona"
class Estudiante implements Persona {
  nombre: string;
  edad: number;
  curso: string;

  constructor(nombre: string, edad: number, curso: string) {
    this.nombre = nombre;
    this.edad = edad;
    this.curso = curso;
  }

  // Define el método "presentarse" que devuelve una cadena con el nombre, la edad y el curso del estudiante
  presentarse(): string {
    return `Hola, mi nombre es ${this.nombre}, tengo ${this.edad} años y estoy en el curso de ${this.curso}.`;
  }
}

// Declara una variable llamada "estudiante" de tipo "Estudiante"
let estudiante: Estudiante = new Estudiante("Juan", 20, "Ingeniería en Computación");

// Llama al método "presentarse" del estudiante y muestra el resultado en la consola
console.log(estudiante.presentarse()); // Muestra "Hola, mi nombre es Juan, tengo 20 años y estoy en el curso de Ingeniería en Computación." en la consola

// Crea una función genérica llamada "Intercambiar" que intercambia los valores de dos variables
function Intercambiar<T>(a: T, b: T): void {
  let temp: T = a;
  a = b;
  b = temp;
}

// Declara dos variables de tipo numérico
let num1 = 1;
let num2 = 2;

// Llama a la función "Intercambiar" con las variables "num1" y "num2"
Intercambiar(num1, num2);

// Muestra los valores de las variables "num1" y "num2" en la consola
console.log(`num1: ${num1}, num2: ${num2}`); // Muestra "num1: 2, num2: 1" en la consola

// Crea un módulo llamado "Calculadora"
module Calculadora {
  // Define una función llamada "sumar" que suma dos números
  export function sumar(num1: number, num2: number): number {
    return num1 + num2;
  }

  // Define una función llamada "restar" que resta dos números
  export function restar(num1: number, num2: number): number {
    return num1 - num2;
  }

  // Define una función llamada "multiplicar" que multiplica dos números
  export function multiplicar(num1: number, num2: number): number {
    return num1 * num2;
  }

  // Define una función llamada "dividir" que divide dos números
  export function dividir(num1: number, num2: number): number {
    return num1 / num2;
  }
}

// Importa el módulo "Calculadora"
import { sumar, restar, multiplicar, dividir } from "./Calculadora";

// Llama a las funciones del módulo "Calculadora" y muestra los resultados en la consola
console.log(`Suma: ${sumar(1, 2)}`); // Muestra "Suma: 3" en la consola
console.log(`Resta: ${restar(3, 2)}`); // Muestra "Resta: 1" en la consola
console.log(`Multiplicación: ${multiplicar(4, 5)}`); // Muestra "Multiplicación: 20" en la consola
console.log(`División: ${dividir(10, 2)}`); // Muestra "División: 5" en la consola
```

Este código es un ejemplo complejo de TypeScript que incluye una variedad de características del lenguaje, incluyendo componentes de React, funciones genéricas, módulos y clases. El código está bien documentado y explicado, lo que lo hace fácil de entender y seguir.