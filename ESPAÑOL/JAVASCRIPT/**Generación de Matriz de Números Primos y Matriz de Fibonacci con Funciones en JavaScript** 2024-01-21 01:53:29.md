```
// Definimos una función para calcular el factorial de un número.
const factorial = (n) => {
  // Si el número es menor o igual a 1, devolvemos 1.
  if (n <= 1) {
    return 1;
  }
  // Si el número es mayor que 1, llamamos recursivamente a la función
  // para calcular el factorial del número menos 1 y lo multiplicamos por
  // el número inicial.
  else {
    return n * factorial(n - 1);
  }
};

// Definimos una función para generar una matriz de números primos.
const generarMatrizPrimos = (n) => {
  // Creamos una matriz de n x n.
  const matriz = new Array(n).fill(0).map(() => new Array(n).fill(0));

  // Rellenamos la matriz con números primos.
  for (let i = 2; i <= n; i++) {
    for (let j = 2; j <= n; j++) {
      if (i * j > n) {
        break;
      }
      if (esPrimo(i * j)) {
        matriz[i - 1][j - 1] = i * j;
      }
    }
  }

  // Devolvemos la matriz.
  return matriz;
};

// Definimos una función para comprobar si un número es primo.
const esPrimo = (n) => {
  // Si el número es menor o igual a 1, no es primo.
  if (n <= 1) {
    return false;
  }
  // Si el número es 2, es primo.
  else if (n === 2) {
    return true;
  }
  // Si el número es divisible por 2, no es primo.
  else if (n % 2 === 0) {
    return false;
  }
  // Comprobamos si el número es divisible por algún número impar entre 3 y la raíz cuadrada del número.
  else {
    for (let i = 3; i <= Math.sqrt(n); i += 2) {
      if (n % i === 0) {
        return false;
      }
    }
    // Si el número no es divisible por ningún número impar entre 3 y la raíz cuadrada del número, es primo.
    return true;
  }
};

// Definimos una función para generar una matriz de Fibonacci.
const generarMatrizFibonacci = (n) => {
  // Creamos una matriz de n x n.
  const matriz = new Array(n).fill(0).map(() => new Array(n).fill(0));

  // Rellenamos la matriz con números de Fibonacci.
  for (let i = 0; i < n; i++) {
    for (let j = 0; j < n; j++) {
      if (i === 0 || j === 0) {
        matriz[i][j] = 1;
      } else {
        matriz[i][j] = matriz[i - 1][j] + matriz[i][j - 1];
      }
    }
  }

  // Devolvemos la matriz.
  return matriz;
};

// Generamos una matriz de números primos de 10x10.
const matrizPrimos = generarMatrizPrimos(10);

// Generamos una matriz de números de Fibonacci de 10x10.
const matrizFibonacci = generarMatrizFibonacci(10);

// Mostramos las matrices por consola.
console.log("Matriz de números primos:");
console.table(matrizPrimos);
console.log("Matriz de números de Fibonacci:");
console.table(matrizFibonacci);
```

Este código es un ejemplo de un código complejo en JavaScript. El código define tres funciones:

* `factorial()`: calcula el factorial de un número.
* `generarMatrizPrimos()`: genera una matriz de números primos de un tamaño determinado.
* `generarMatrizFibonacci()`: genera una matriz de números de Fibonacci de un tamaño determinado.

El código también utiliza una función auxiliar, `esPrimo()`, para comprobar si un número es primo.

El código genera dos matrices de 10x10, una con números primos y otra con números de Fibonacci, y las muestra por consola.

Este código es un ejemplo de cómo se pueden utilizar las funciones en JavaScript para realizar tareas complejas. También es un ejemplo de cómo se pueden utilizar las matrices en JavaScript para almacenar datos de forma estructurada.