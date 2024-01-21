```javascript
// Código complejo en JavaScript

// Definición de una función recursiva para calcular el factorial de un número
function factorial(numero) {
  if (numero === 1) {
    return 1;
  } else {
    return numero * factorial(numero - 1);
  }
}

// Definición de una función para generar un número aleatorio dentro de un rango
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Definición de una función para encontrar el elemento máximo en un array
function encontrarMaximo(array) {
  let max = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > max) {
      max = array[i];
    }
  }
  return max;
}

// Definición de una función para encontrar el elemento mínimo en un array
function encontrarMinimo(array) {
  let min = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < min) {
      min = array[i];
    }
  }
  return min;
}

// Definición de una función para ordenar un array en orden ascendente
function ordenarAscendente(array) {
  array.sort((a, b) => a - b);
  return array;
}

// Definición de una función para ordenar un array en orden descendente
function ordenarDescendente(array) {
  array.sort((a, b) => b - a);
  return array;
}

// Definición de una función para invertir un array
function invertirArray(array) {
  let arrayInvertido = [];
  for (let i = array.length - 1; i >= 0; i--) {
    arrayInvertido.push(array[i]);
  }
  return arrayInvertido;
}

// Definición de una función para encontrar el índice de un elemento en un array
function encontrarIndice(array, elemento) {
  return array.indexOf(elemento);
}

// Definición de una función para eliminar un elemento de un array por su índice
function eliminarElementoPorIndice(array, indice) {
  array.splice(indice, 1);
  return array;
}

// Definición de una función para eliminar un elemento de un array por su valor
function eliminarElementoPorValor(array, elemento) {
  let indice = array.indexOf(elemento);
  if (indice !== -1) {
    array.splice(indice, 1);
  }
  return array;
}

// Definición de una función para añadir un elemento al final de un array
function añadirElementoAlFinal(array, elemento) {
  array.push(elemento);
  return array;
}

// Definición de una función para añadir un elemento al principio de un array
function añadirElementoAlPrincipio(array, elemento) {
  array.unshift(elemento);
  return array;
}

// Definición de una función para buscar un elemento en un array
function buscarElemento(array, elemento) {
  return array.includes(elemento);
}

// Definición de una función para comprobar si un array está vacío
function estaVacio(array) {
  return array.length === 0;
}

// Definición de una función para obtener el tamaño de un array
function obtenerTamaño(array) {
  return array.length;
}

// Definición de una función para crear un objeto vacío
function crearObjetoVacio() {
  let objeto = {};
  return objeto;
}

// Definición de una función para añadir una propiedad a un objeto
function añadirPropiedad(objeto, propiedad, valor) {
  objeto[propiedad] = valor;
  return objeto;
}

// Definición de una función para eliminar una propiedad de un objeto
function eliminarPropiedad(objeto, propiedad) {
  delete objeto[propiedad];
  return objeto;
}

// Definición de una función para obtener el valor de una propiedad de un objeto
function obtenerValorPropiedad(objeto, propiedad) {
  return objeto[propiedad];
}

// Definición de una función para comprobar si un objeto está vacío
function estaVacio(objeto) {
  return Object.keys(objeto).length === 0;
}

// Definición de una función para obtener el tamaño de un objeto
function obtenerTamaño(objeto) {
  return Object.keys(objeto).length;
}

// Definición de una función para crear un array vacío
function crearArrayVacio() {
  let array = [];
  return array;
}

// Definición de una función para añadir un elemento al final de un array
function añadirElementoAlFinal(array, elemento) {
  array.push(elemento);
  return array;
}

// Definición de una función para añadir un elemento al principio de un array
function añadirElementoAlPrincipio(array, elemento) {
  array.unshift(elemento);
  return array;
}

// Defin