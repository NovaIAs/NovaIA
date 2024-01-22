```javascript
// Definición de una función recursiva para calcular el factorial de un número.
const factorial = (n) => {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
};

// Definición de una función para generar una tabla de multiplicar de un número dado.
const tablaDeMultiplicar = (n) => {
  for (let i = 1; i <= 10; i++) {
    console.log(`${n} x ${i} = ${n * i}`);
  }
};

// Definición de una función para encontrar el máximo común divisor de dos números.
const maximoComunDivisor = (a, b) => {
  while (b !== 0) {
    let temp = b;
    b = a % b;
    a = temp;
  }
  return a;
};

// Definición de una función para encontrar el mínimo común múltiplo de dos números.
const minimoComunMultiplo = (a, b) => {
  return (a * b) / maximoComunDivisor(a, b);
};

// Definición de una función para generar un número aleatorio entre dos números dados.
const numeroAleatorio = (min, max) => {
  return Math.floor(Math.random() * (max - min + 1)) + min;
};

// Definición de una función para ordenar un array de números en orden ascendente.
const ordenarArray = (array) => {
  array.sort((a, b) => a - b);
  return array;
};

// Definición de una función para eliminar los elementos duplicados de un array.
const eliminarDuplicados = (array) => {
  return [...new Set(array)];
};

// Definición de una función para encontrar el elemento más grande en un array.
const elementoMasGrande = (array) => {
  return Math.max(...array);
};

// Definición de una función para encontrar el elemento más pequeño en un array.
const elementoMasPequeno = (array) => {
  return Math.min(...array);
};

// Definición de una función para calcular la suma de los elementos de un array.
const sumaDeElementos = (array) => {
  return array.reduce((a, b) => a + b, 0);
};

// Definición de una función para calcular el promedio de los elementos de un array.
const promedioDeElementos = (array) => {
  return sumaDeElementos(array) / array.length;
};

// Definición de una función para comprobar si un número es primo.
const esPrimo = (n) => {
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

// Definición de una función para encontrar todos los números primos hasta un número dado.
const encontrarPrimos = (n) => {
  const primos = [];
  for (let i = 2; i <= n; i++) {
    if (esPrimo(i)) {
      primos.push(i);
    }
  }
  return primos;
};

```

Explicación:

* La función `factorial` calcula el factorial de un número utilizando la recursividad.
* La función `tablaDeMultiplicar` genera una tabla de multiplicar de un número dado.
* La función `maximoComunDivisor` encuentra el máximo común divisor de dos números.
* La función `minimoComunMultiplo` encuentra el mínimo común múltiplo de dos números.
* La función `numeroAleatorio` genera un número aleatorio entre dos números dados.
* La función `ordenarArray` ordena un array de números en orden ascendente.
* La función `eliminarDuplicados` elimina los elementos duplicados de un array.
* La función `elementoMasGrande` encuentra el elemento más grande en un array.
* La función `elementoMasPequeno` encuentra el elemento más pequeño en un array.
* La función `sumaDeElementos` calcula la suma de los elementos de un array.
* La función `promedioDeElementos` calcula el promedio de los elementos de un array.
* La función `esPrimo` comprueba si un número es primo.
* La función `encontrarPrimos` encuentra todos los números primos hasta un número dado.