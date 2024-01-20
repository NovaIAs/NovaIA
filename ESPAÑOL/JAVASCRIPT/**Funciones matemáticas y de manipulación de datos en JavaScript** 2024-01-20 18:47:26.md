```javascript
// Función recursiva para calcular el factorial de un número.
const factorial = (n) => {
  if (n === 1) {
    return 1;
  }
  return n * factorial(n - 1);
};

// Función para encontrar el número mayor en un array.
const findMax = (arr) => {
  if (arr.length === 1) {
    return arr[0];
  }
  const max = findMax(arr.slice(1));
  return arr[0] > max ? arr[0] : max;
};

// Función para encontrar la posición de un elemento en un array.
const findIndex = (arr, target) => {
  for (let i = 0; i < arr.length; i++) {
    if (arr[i] === target) {
      return i;
    }
  }
  return -1;
};

// Función para invertir una cadena de texto.
const reverseString = (str) => {
  if (str.length === 1) {
    return str;
  }
  return reverseString(str.slice(1)) + str[0];
};

// Función para comprobar si una cadena de texto es un palíndromo.
const isPalindrome = (str) => {
  if (str.length === 1) {
    return true;
  }
  if (str[0] !== str[str.length - 1]) {
    return false;
  }
  return isPalindrome(str.slice(1, str.length - 1));
};

// Función para ordenar un array de números de menor a mayor.
const sortAscending = (arr) => {
  if (arr.length <= 1) {
    return arr;
  }
  const sorted = sortAscending(arr.slice(1));
  const current = arr[0];
  for (let i = sorted.length - 1; i >= 0; i--) {
    if (sorted[i] > current) {
      sorted.splice(i + 1, 0, current);
      break;
    }
  }
  return sorted;
};

// Función para ordenar un array de números de mayor a menor.
const sortDescending = (arr) => {
  if (arr.length <= 1) {
    return arr;
  }
  const sorted = sortDescending(arr.slice(1));
  const current = arr[0];
  for (let i = 0; i < sorted.length; i++) {
    if (sorted[i] < current) {
      sorted.splice(i, 0, current);
      break;
    }
  }
  return sorted;
};

// Función para generar un número aleatorio entre un rango de números.
const generateRandomNumber = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

// Función para comprobar si un número es primo.
const isPrime = (n) => {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
};

// Función para encontrar el máximo común divisor de dos números.
const findGCD = (a, b) => {
  if (a === 0) {
    return b;
  }
  return findGCD(b % a, a);
};

// Función para encontrar el mínimo común múltiplo de dos números.
const findLCM = (a, b) => {
  return (a * b) / findGCD(a, b);
};

// Función para encontrar las raíces de una ecuación cuadrática.
const findQuadraticRoots = (a, b, c) => {
  const discriminant = b * b - 4 * a * c;
  if (discriminant < 0) {
    return []; // No hay raíces reales
  }
  const x1 = (-b + Math.sqrt(discriminant)) / (2 * a);
  const x2 = (-b - Math.sqrt(discriminant)) / (2 * a);
  return [x1, x2];
};

// Función para encontrar la suma de los dígitos de un número.
const sumDigits = (n) => {
  if (n === 0) {
    return 0;
  }
  return n % 10 + sumDigits(Math.floor(n / 10));
};

// Función para encontrar el número de dígitos de un número.
const countDigits = (n) => {
  if (n === 0) {
    return 0;
  }
  return 1 + countDigits(Math.floor(n / 10));
};
```

Explicación del código:

* La función `factorial` calcula el factorial de un número usando recursión.
* La función `findMax` encuentra el número mayor en un array usando recursión.
* La función `findIndex` encuentra la posición de un elemento en un array usando un bucle `for`.
* La función `reverseString` invierte una cadena de texto usando recursión.
* La función `isPalindrome` comprueba si una cadena de texto es un palíndromo usando recursión.
* La función `sortAscending` ordena un array de números de menor a mayor usando el algoritmo de inserción.
* La función `sortDescending` ordena un array de números de mayor a menor usando el algoritmo de inserción.
* La función `generateRandomNumber` genera un número aleatorio entre un rango de números.
* La función `isPrime` comprueba si un número es primo usando un bucle `for`.
* La función `findGCD` encuentra el máximo común divisor de dos números usando el algoritmo de Euclides.
* La función `findLCM` encuentra el mínimo común múltiplo de dos números usando el máximo común divisor.
* La función `findQuadraticRoots` encuentra las raíces de una ecuación cuadrática usando la fórmula cuadrática.
* La función `sumDigits` encuentra la suma de los dígitos de un número usando recursión.
* La función `countDigits` encuentra el número de dígitos de un número usando recursión.