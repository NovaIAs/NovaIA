```typescript

// Objetos con Tipos y Generics

type Persona = {
  nombre: string;
  edad: number;
  ocupación?: string; // Campo opcional
};

type ListaPersonas<T> = Array<T>; // Tipo genérico para una lista de cualquier tipo

const listaPersonas: ListaPersonas<Persona> = [
  { nombre: 'Juan', edad: 25, ocupación: 'Programador' },
  { nombre: 'María', edad: 30, ocupación: 'Abogada' }
];

listaPersonas.forEach(persona => {
  console.log(`Nombre: ${persona.nombre}, Edad: ${persona.edad}, Ocupación: ${persona.ocupación || 'Sin ocupación'}`);
});

// Clases e Interfaces

interface Vehículo {
  marca: string;
  modelo: string;
  año: number;
}

class Coche implements Vehículo {
  marca: string;
  modelo: string;
  año: number;
  color: string;

  constructor(marca: string, modelo: string, año: number, color: string) {
    this.marca = marca;
    this.modelo = modelo;
    this.año = año;
    this.color = color;
  }

  getInfo() {
    return `Marca: ${this.marca}, Modelo: ${this.modelo}, Año: ${this.año}, Color: ${this.color}`;
  }
}

const coche1 = new Coche('Toyota', 'Corolla', 2020, 'Rojo');
console.log(coche1.getInfo());

// Funciones Genéricas

function esIgual<T>(a: T, b: T): boolean {
  return a === b;
}

console.log(esIgual(1, 2)); // false
console.log(esIgual('Hola', 'Hola')); // true

// Promesas y Async/Await

const obtenerDatos = async () => {
  const respuesta = await fetch('https://example.com/api/datos');
  const datos = await respuesta.json();
  return datos;
};

obtenerDatos().then(datos => {
  console.log(datos);
}).catch(error => {
  console.error(error);
});

// Generadores

function* generarNumeros() {
  let contador = 0;
  while (true) {
    yield contador++;
  }
}

const generador = generarNumeros();

console.log(generador.next().value); // 0
console.log(generador.next().value); // 1
console.log(generador.next().value); // 2

// Expresiones Regulares

const regex = /^(?:\+|-)?\d+/; // Expresión regular para números enteros

const resultado = regex.test('123'); // true
const resultado2 = regex.test('-456'); // true
const resultado3 = regex.test('abc'); // false

// Métodos de Arrays

const numeros = [1, 2, 3, 4, 5];

console.log(numeros.map(n => n * 2)); // [2, 4, 6, 8, 10]
console.log(numeros.filter(n => n % 2 === 0)); // [2, 4]
console.log(numeros.reduce((acc, cur) => acc + cur, 0)); // 15

```

Explicación:

- El código define tipos, interfaces, clases, funciones genéricas, promesas, generadores, expresiones regulares y métodos de arrays con comentarios en español para explicar cada parte.


- Imprime los datos obtenidos de una API utilizando promesas y async/await.


- Utiliza un generador para generar una secuencia de números.


- Utiliza una expresión regular para validar números enteros.


- Demuestra el uso de métodos de arrays como map, filter y reduce.