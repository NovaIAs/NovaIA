```javascript
// Función para calcular el factorial de un número

function factorial(numero) {
  if (numero < 0) {
    throw new Error("El número debe ser positivo.");
  }
  if (numero === 0) {
    return 1;
  }
  let resultado = 1;
  for (let i = 1; i <= numero; i++) {
    resultado *= i;
  }
  return resultado;
}

// Función para generar un número aleatorio entre dos números

function numeroAleatorio(minimo, maximo) {
  if (minimo > maximo) {
    throw new Error("El número mínimo debe ser menor o igual al número máximo.");
  }
  return Math.floor(Math.random() * (maximo - minimo + 1)) + minimo;
}

// Función para invertir una cadena de caracteres

function invertirCadena(cadena) {
  if (typeof cadena !== "string") {
    throw new Error("El argumento debe ser una cadena de caracteres.");
  }
  let cadenaInvertida = "";
  for (let i = cadena.length - 1; i >= 0; i--) {
    cadenaInvertida += cadena[i];
  }
  return cadenaInvertida;
}

// Función para contar el número de apariciones de un elemento en un array

function contarApariciones(array, elemento) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  let contador = 0;
  for (let i = 0; i < array.length; i++) {
    if (array[i] === elemento) {
      contador++;
    }
  }
  return contador;
}

// Función para ordenar un array de números de forma ascendente

function ordenarAscendente(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  array.sort((a, b) => a - b);
  return array;
}

// Función para ordenar un array de números de forma descendente

function ordenarDescendente(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  array.sort((a, b) => b - a);
  return array;
}

// Función para comprobar si un array está vacío

function estaVacio(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  return array.length === 0;
}

// Función para eliminar los elementos duplicados de un array

function eliminarDuplicados(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  return [...new Set(array)];
}

// Función para encontrar el elemento máximo de un array

function elementoMaximo(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  if (estaVacio(array)) {
    throw new Error("El array está vacío.");
  }
  let maximo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maximo) {
      maximo = array[i];
    }
  }
  return maximo;
}

// Función para encontrar el elemento mínimo de un array

function elementoMinimo(array) {
  if (!Array.isArray(array)) {
    throw new Error("El argumento debe ser un array.");
  }
  if (estaVacio(array)) {
    throw new Error("El array está vacío.");
  }
  let minimo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < minimo) {
      minimo = array[i];
    }
  }
  return minimo;
}

// Función para calcular la suma de los elementos de un array