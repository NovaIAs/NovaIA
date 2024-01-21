**Código Complejo en JavaScript**

```javascript
// Función para calcular la suma de los números de un array
function sumarArray(array) {
  let suma = 0;
  for (let i = 0; i < array.length; i++) {
    suma += array[i];
  }
  return suma;
}

// Función para calcular el producto de los números de un array
function multiplicarArray(array) {
  let producto = 1;
  for (let i = 0; i < array.length; i++) {
    producto *= array[i];
  }
  return producto;
}

// Función para calcular el máximo de los números de un array
function maximoArray(array) {
  let maximo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maximo) {
      maximo = array[i];
    }
  }
  return maximo;
}

// Función para calcular el mínimo de los números de un array
function minimoArray(array) {
  let minimo = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < minimo) {
      minimo = array[i];
    }
  }
  return minimo;
}

// Función para calcular el promedio de los números de un array
function promedioArray(array) {
  let suma = sumarArray(array);
  let promedio = suma / array.length;
  return promedio;
}

// Función para calcular la mediana de los números de un array
function medianaArray(array) {
  // Ordenar el array en orden ascendente
  array.sort((a, b) => a - b);

  // Calcular el índice de la mediana
  let medianaIndex = Math.floor(array.length / 2);

  // Si el array tiene un número par de elementos, la mediana es el promedio de los dos elementos centrales
  if (array.length % 2 === 0) {
    return (array[medianaIndex] + array[medianaIndex - 1]) / 2;
  }

  // Si el array tiene un número impar de elementos, la mediana es el elemento central
  else {
    return array[medianaIndex];
  }
}

// Función para calcular la desviación estándar de los números de un array
function desviacionEstandarArray(array) {
  // Calcular la media del array
  let media = promedioArray(array);

  // Calcular la suma de las diferencias cuadradas entre cada elemento del array y la media
  let sumaDiferenciasCuadradas = 0;
  for (let i = 0; i < array.length; i++) {
    sumaDiferenciasCuadradas += Math.pow(array[i] - media, 2);
  }

  // Calcular la varianza del array
  let varianza = sumaDiferenciasCuadradas / (array.length - 1);

  // Calcular la desviación estándar del array
  let desviacionEstandar = Math.sqrt(varianza);

  return desviacionEstandar;
}

// Función para generar un número aleatorio entre dos valores
function generarNumeroAleatorio(min, max) {
  return Math.random() * (max - min) + min;
}

// Función para generar un array de números aleatorios de una longitud determinada
function generarArrayAleatorio(longitud, min, max) {
  let array = [];
  for (let i = 0; i < longitud; i++) {
    array.push(generarNumeroAleatorio(min, max));
  }
  return array;
}

// Función para encontrar el elemento más frecuente en un array
function elementoMasFrecuenteArray(array) {
  // Crear un objeto para almacenar las frecuencias de los elementos del array
  let frecuencias = {};
  for (let i = 0; i < array.length; i++) {
    if (frecuencias[array[i]] === undefined) {
      frecuencias[array[i]] = 0;
    }
    frecuencias[array[i]]++;
  }

  // Encontrar el elemento con la frecuencia más alta
  let elementoMasFrecuente = null;
  let frecuenciaMasAlta = 0;
  for (let elemento in frecuencias) {
    if (frecuencias[elemento] > frecuenciaMasAlta) {
      elementoMasFrecuente = elemento;
      frecuenciaMasAlta = frecuencias[elemento];
    }
  }

  return elementoMasFrecuente;
}

// Función para eliminar los elementos duplicados de un array
function eliminarDuplicadosArray(array) {
  // Crear un conjunto para almacenar los elementos únicos del array
  let conjunto = new Set();
  for (let i = 0; i < array.length; i++) {
    conjunto.add(array[i]);
  }

  // Convertir el conjunto en un array
  let arraySinDuplicados = Array.from(conjunto);

  return arraySinDuplicados;
}

// Función para invertir el orden de los elementos de un array
function invertirArray(array) {
  let arrayInverso = [];
  for (let i = array.length - 1; i >= 0; i--) {
    arrayInverso.push(array[i]);
  }
  return arrayInverso;
}

// Función para rotar los elementos de un array una posición a la derecha
function rotarArrayDerecha(array) {
  let ultimoElemento = array[array.length - 1];
  for (let i = array.length - 1; i > 0; i--) {
    array[i] = array[i - 1];
  }
  array[0] = ultimoElemento;
  return array;
}

// Función para rotar los elementos de un array una posición a la izquierda
function rotarArrayIzquierda(array) {
  let primerElemento = array[0];
  for (let i = 0; i < array.length - 1