```typescript
// Función que recibe dos números y devuelve su suma.
function sumar(a: number, b: number): number {
  return a + b;
}

// 2. Función que recibe un array de números y un número, y devuelve el índice de la primera ocurrencia del número en el array.
function encontrarIndice(array: number[], numero: number): number {
  for (let i = 0; i < array.length; i++) {
    if (array[i] === numero) {
      return i;
    }
  }

  return -1; // Si el número no se encuentra en el array, se devuelve -1.
}

// 3. Función que recibe un array de objetos, una clave y un valor, y devuelve un nuevo array con los objetos que tienen la clave y el valor especificados.
function filtrarObjetos(
  array: object[],
  clave: string,
  valor: any
): object[] {
  return array.filter((objeto) => objeto[clave] === valor);
}

// 4. Clase que representa a una persona.
class Persona {
  nombre: string;
  edad: number;

  constructor(nombre: string, edad: number) {
    this.nombre = nombre;
    this.edad = edad;
  }

  saludar() {
    console.log(`Hola, mi nombre es ${this.nombre} y tengo ${this.edad} años.`);
  }
}

// 5. Clase que representa a un empleado.
class Empleado extends Persona {
  salario: number;

  constructor(nombre: string, edad: number, salario: number) {
    super(nombre, edad);
    this.salario = salario;
  }

  trabajar() {
    console.log(`${this.nombre} está trabajando.`);
  }
}

// 6. Creamos una función que genere un número aleatorio entre dos números.
function generarNumeroAleatorio(min: number, max: number): number {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// 7. Creamos una función que reciba un array y devuelva una nueva lista de listas.
function agruparElementos(array: number[], tamaño: number): number[][] {
  const resultado = [];

  for (let i = 0; i < array.length; i += tamaño) {
    resultado.push(array.slice(i, i + tamaño));
  }

  return resultado;
}

// Ejemplo de uso de las funciones definidas anteriormente.

// Sumamos dos números.
const sumaResultado = sumar(5, 10);
console.log(`La suma de 5 y 10 es ${sumaResultado}.`);

// Buscamos el índice de un número en un array.
const indice = encontrarIndice([1, 2, 3, 4, 5], 3);
if (indice !== -1) {
  console.log(`El número 3 se encuentra en el índice ${indice}.`);
} else {
  console.log(`El número 3 no se encuentra en el array.`);
}

// Filtramos un array de objetos por una clave y un valor.
const objetosFiltrados = filtrarObjetos(
  [
    { id: 1, nombre: "Juan", edad: 25 },
    { id: 2, nombre: "María", edad: 30 },
    { id: 3, nombre: "Pedro", edad: 35 },
  ],
  "nombre",
  "María"
);
console.log(objetosFiltrados); // [{ id: 2, nombre: "María", edad: 30 }]

// Creamos un objeto de tipo Persona.
const persona = new Persona("Juan", 25);
persona.saludar(); // Hola, mi nombre es Juan y tengo 25 años.

// Creamos un objeto de tipo Empleado.
const empleado = new Empleado("María", 30, 1000);
empleado.saludar(); // Hola, mi nombre es María y tengo 30 años.
empleado.trabajar(); // María está trabajando.

// Generamos un número aleatorio entre 1 y 10.
const numeroAleatorio = generarNumeroAleatorio(1, 10);
console.log(`El número aleatorio generado es ${numeroAleatorio}.`);

// Agrupamos un array de números en listas de tamaño 3.
const listasAgrupadas = agruparElementos([1, 2, 3, 4, 5, 6, 7, 8, 9], 3);
console.log(listasAgrupadas); // [[1, 2, 3], [4, 5, 6], [7, 8, 9]]
```

Explicación del código:

1. La función `sumar` recibe dos números como parámetros y devuelve la suma de ambos números.

2. La función `encontrarIndice` recibe un array de números y un número como parámetros y devuelve el índice de la primera ocurrencia del número en el array. Si el número no se encuentra en el array, devuelve -1.

3. La función `filtrarObjetos` recibe un array de objetos, una clave y un valor como parámetros y devuelve un nuevo array con los objetos que tienen la clave y el valor especificados.

4. La clase `Persona` representa a una persona. Tiene dos propiedades: `nombre` y `edad`. También tiene un método llamado `saludar()` que imprime un mensaje de saludo con el nombre y la edad de la persona.

5. La clase `Empleado` es una subclase de la clase `Persona`. Tiene una propiedad adicional llamada `salario`. También tiene un método llamado `trabajar()` que imprime un mensaje de que el empleado está trabajando.

6. La función `generarNumeroAleatorio` recibe dos números como parámetros y devuelve un número aleatorio entre los dos números especificados.

7. La función `agruparElementos` recibe un array y un tamaño como parámetros y devuelve una nueva lista de listas. Cada lista de la lista resultante contiene un número de elementos igual al tamaño especificado.

El ejemplo de uso del código muestra cómo utilizar las funciones y clases definidas anteriormente.