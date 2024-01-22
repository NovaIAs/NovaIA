```typescript
// Definimos una función que recibirá un array de números y devolverá el promedio de todos los números del array.
export function promedio(numeros: number[]): number {
  // Sumamos todos los números del array y los guardamos en la variable "suma".
  let suma = 0;
  for (let i = 0; i < numeros.length; i++) {
    suma += numeros[i];
  }

  // Dividimos la suma de los números por la cantidad de números del array y la guardamos en la variable "promedio".
  const promedio = suma / numeros.length;

  // Devolvemos el promedio.
  return promedio;
}

// Definimos una función que recibirá un array de strings y devolverá un nuevo array con todos los strings en mayúsculas.
export function aMayusculas(palabras: string[]): string[] {
  // Creamos un nuevo array vacío para guardar los strings en mayúsculas.
  const palabrasMayusculas: string[] = [];

  // Recorremos el array de strings y añadimos cada string a "palabrasMayusculas" convertido a mayúsculas.
  for (let i = 0; i < palabras.length; i++) {
    palabrasMayusculas.push(palabras[i].toUpperCase());
  }

  // Devolvemos el array con los strings en mayúsculas.
  return palabrasMayusculas;
}

// Definimos una función que recibirá un objeto y devolverá un nuevo objeto con las claves y valores invertidos.
export function invertirObjeto(objeto: object): object {
  // Creamos un nuevo objeto para guardar el objeto invertido.
  const objetoInvertido: object = {};

  // Recorremos las claves del objeto y las añadimos al objeto invertido como valores, y los valores del objeto como claves.
  for (const key in objeto) {
    objetoInvertido[objeto[key]] = key;
  }

  // Devolvemos el objeto invertido.
  return objetoInvertido;
}

// Definimos una función que recibirá un número y devolverá un array con todos los divisores del número.
export function divisores(numero: number): number[] {
  // Creamos un array vacío para guardar los divisores del número.
  const divisores: number[] = [];

  // Recorremos todos los números desde 1 hasta el número y comprobamos si son divisores del número.
  for (let i = 1; i <= numero; i++) {
    if (numero % i === 0) {
      // Si el número es divisible por i, lo añadimos al array de divisores.
      divisores.push(i);
    }
  }

  // Devolvemos el array de divisores.
  return divisores;
}

// Definimos una función que recibirá un string y devolverá un boolean indicando si el string es un palíndromo.
export function esPalindromo(palabra: string): boolean {
  // Convertimos la palabra a minúsculas.
  palabra = palabra.toLowerCase();

  // Comprobamos si la palabra es igual a su inverso.
  return palabra === palabra.split('').reverse().join('');
}
```

Este código es una colección de cinco funciones útiles que realizan diversas tareas. Las funciones son:

* `promedio`: calcula el promedio de los números en un array.
* `aMayusculas`: convierte todos los strings en un array a mayúsculas.
* `invertirObjeto`: invierte las claves y valores de un objeto.
* `divisores`: genera un array con todos los divisores de un número.
* `esPalindromo`: comprueba si un string es un palíndromo.

Estas funciones son todas muy útiles y pueden ser utilizadas en una variedad de aplicaciones. Por ejemplo, la función `promedio` puede ser utilizada para calcular la calificación promedio de un estudiante, la función `aMayusculas` puede ser utilizada para convertir los nombres de los usuarios a mayúsculas, la función `invertirObjeto` puede ser utilizada para crear un objeto con las claves y valores invertidos, la función `divisores` puede ser utilizada para encontrar todos los divisores de un número, y la función `esPalindromo` puede ser utilizada para comprobar si una palabra es un palíndromo.

Estas funciones son sólo algunos ejemplos de las muchas funciones útiles que se pueden crear en TypeScript. TypeScript es un lenguaje de programación muy potente y versátil, y puede ser utilizado para crear una amplia variedad de aplicaciones.