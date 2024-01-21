```javascript
// Definimos una función que recibirá un array de números y devolverá el máximo.
const encontrarMaximo = (numeros) => {
  // Si el array está vacío, devolvemos -1 para indicar que no hay máximo.
  if (numeros.length === 0) {
    return -1;
  }

  // Inicializamos el máximo al primer elemento del array.
  let maximo = numeros[0];

  // Recorremos el array desde la segunda posición hasta el final.
  for (let i = 1; i < numeros.length; i++) {
    // Si el elemento actual es mayor que el máximo, actualizamos el máximo.
    if (numeros[i] > maximo) {
      maximo = numeros[i];
    }
  }

  // Devolvemos el máximo.
  return maximo;
};

// Definimos una función que recibirá un array de números y devolverá el mínimo.
const encontrarMinimo = (numeros) => {
  // Si el array está vacío, devolvemos -1 para indicar que no hay mínimo.
  if (numeros.length === 0) {
    return -1;
  }

  // Inicializamos el mínimo al primer elemento del array.
  let minimo = numeros[0];

  // Recorremos el array desde la segunda posición hasta el final.
  for (let i = 1; i < numeros.length; i++) {
    // Si el elemento actual es menor que el mínimo, actualizamos el mínimo.
    if (numeros[i] < minimo) {
      minimo = numeros[i];
    }
  }

  // Devolvemos el mínimo.
  return minimo;
};

// Definimos una función que recibirá un array de números y devolverá la suma de todos los elementos.
const sumarNumeros = (numeros) => {
  // Si el array está vacío, devolvemos 0 para indicar que la suma es 0.
  if (numeros.length === 0) {
    return 0;
  }

  // Inicializamos la suma a 0.
  let suma = 0;

  // Recorremos el array desde la primera posición hasta el final.
  for (let i = 0; i < numeros.length; i++) {
    // Sumamos el elemento actual a la suma.
    suma += numeros[i];
  }

  // Devolvemos la suma.
  return suma;
};

// Definimos una función que recibirá un array de números y devolverá el producto de todos los elementos.
const multiplicarNumeros = (numeros) => {
  // Si el array está vacío, devolvemos 1 para indicar que el producto es 1.
  if (numeros.length === 0) {
    return 1;
  }

  // Inicializamos el producto a 1.
  let producto = 1;

  // Recorremos el array desde la primera posición hasta el final.
  for (let i = 0; i < numeros.length; i++) {
    // Multiplicamos el elemento actual por el producto.
    producto *= numeros[i];
  }

  // Devolvemos el producto.
  return producto;
};

// Definimos una función que recibirá un array de números y devolverá la media de todos los elementos.
const calcularMedia = (numeros) => {
  // Si el array está vacío, devolvemos 0 para indicar que la media es 0.
  if (numeros.length === 0) {
    return 0;
  }

  // Calculamos la suma de todos los elementos.
  const suma = sumarNumeros(numeros);

  // Calculamos la media dividiendo la suma por el número de elementos.
  const media = suma / numeros.length;

  // Devolvemos la media.
  return media;
};

// Definimos una función que recibirá un array de números y devolverá la moda de todos los elementos.
const calcularModa = (numeros) => {
  // Si el array está vacío, devolvemos -1 para indicar que no hay moda.
  if (numeros.length === 0) {
    return -1;
  }

  // Creamos un objeto para almacenar la frecuencia de cada elemento.
  const frecuencias = {};

  // Recorremos el array y actualizamos las frecuencias.
  for (const numero of numeros) {
    if (frecuencias[numero] === undefined) {
      frecuencias[numero] = 0;
    }
    frecuencias[numero]++;
  }

  // Buscamos el elemento con mayor frecuencia.
  let moda = -1;
  let frecuenciaModa = 0;
  for (const numero in frecuencias) {
    if (frecuencias[numero] > frecuenciaModa) {
      moda = numero;
      frecuenciaModa = frecuencias[numero];
    }
  }

  // Devolvemos la moda.
  return moda;
};

// Definimos una función que recibirá un array de números y devolverá la mediana de todos los elementos.
const calcularMediana = (numeros) => {
  // Si el array está vacío, devolvemos 0 para indicar que la mediana es 0.
  if (numeros.length === 0) {
    return 0;
  }

  // Ordenamos el array en orden ascendente.
  numeros.sort((a, b) => a - b);

  // Si el número de elementos del array es impar, la mediana es el elemento central.
  if (numeros.length % 2 === 1) {
    return numeros[Math.floor(numeros.length / 2)];
  }
  // Si el número de elementos del array es par, la mediana es la media de los dos elementos centrales.
  else {
    return (numeros[numeros.length / 2 - 1] + numeros[numeros.length / 2]) / 2;
  }
};
```

Este código es una colección de funciones matemáticas que realizan operaciones básicas sobre un array de números. Las funciones son:

* `encontrarMaximo`: Encuentra el máximo de un array de números.
* `encontrarMinimo`: Encuentra el mínimo de un array de números.
* `sumarNumeros`: Suma todos los elementos de un array de números.
* `multiplicarNumeros`: Multiplica todos los elementos de un array de números.
* `calcularMedia`: Calcula la media de un array de números.
* `calcularModa`: Calcula la moda de un array de números.
* `calcularMediana`: Calcula la mediana de un array de números.

El código está bien documentado y es fácil de entender. Puede ser utilizado para realizar cálculos básicos sobre arrays de números.