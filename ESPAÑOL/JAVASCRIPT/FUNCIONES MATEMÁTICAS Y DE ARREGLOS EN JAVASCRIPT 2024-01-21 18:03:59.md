```javascript
// Función para calcular el factorial de un número
function factorial(n) {
  if (n < 0) {
    throw new Error("El número debe ser positivo");
  }
  if (n === 0) {
    return 1;
  }
  return n * factorial(n - 1);
}

// Función para generar una matriz de ceros de un tamaño determinado
function crearMatrizDeCeros(filas, columnas) {
  let matriz = [];
  for (let i = 0; i < filas; i++) {
    matriz.push(new Array(columnas).fill(0));
  }
  return matriz;
}

// Función para invertir una cadena de texto
function invertirCadena(cadena) {
  return cadena.split("").reverse().join("");
}

// Función para ordenar un array de números de forma ascendente
function ordenarNumerosAscendente(array) {
  return array.sort((a, b) => a - b);
}

// Función para ordenar un array de números de forma descendente
function ordenarNumerosDescendente(array) {
  return array.sort((a, b) => b - a);
}

// Función para buscar un elemento en un array de forma binaria
function busquedaBinaria(array, elemento) {
  let inicio = 0;
  let fin = array.length - 1;
  while (inicio <= fin) {
    let medio = Math.floor((inicio + fin) / 2);
    if (array[medio] === elemento) {
      return medio;
    } else if (array[medio] < elemento) {
      inicio = medio + 1;
    } else {
      fin = medio - 1;
    }
  }
  return -1;
}

// Función para generar un número aleatorio entre dos valores dados
function generarNumeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Función para crear un objeto con las propiedades de un objeto existente
function crearNuevoObjeto(objeto) {
  return Object.assign({}, objeto);
}

// Función para fusionar dos objetos en uno solo
function fusionarObjetos(objeto1, objeto2) {
  return Object.assign({}, objeto1, objeto2);
}

// Función para obtener el elemento máximo de un array de números
function obtenerMaximo(array) {
  return Math.max(...array);
}

// Función para obtener el elemento mínimo de un array de números
function obtenerMinimo(array) {
  return Math.min(...array);
}

// Función para obtener la suma de los elementos de un array de números
function obtenerSuma(array) {
  return array.reduce((a, b) => a + b, 0);
}

// Función para obtener el producto de los elementos de un array de números
function obtenerProducto(array) {
  return array.reduce((a, b) => a * b, 1);
}

// Función para obtener la media de los elementos de un array de números
function obtenerMedia(array) {
  return obtenerSuma(array) / array.length;
}

// Función para obtener la mediana de los elementos de un array de números
function obtenerMediana(array) {
  array.sort((a, b) => a - b);
  if (array.length % 2 === 0) {
    return (array[array.length / 2] + array[array.length / 2 - 1]) / 2;
  } else {
    return array[Math.floor(array.length / 2)];
  }
}

// Función para obtener la desviación estándar de los elementos de un array de números
function obtenerDesviacionEstandar(array) {
  let media = obtenerMedia(array);
  let varianza =
    array.reduce((a, b) => a + (b - media) ** 2, 0) / (array.length - 1);
  return Math.sqrt(varianza);
}
```

Explicación del código:

* La función `factorial` calcula el factorial de un número.
* La función `crearMatrizDeCeros` genera una matriz de ceros de un tamaño determinado.
* La función `invertirCadena` invierte una cadena de texto.
* La función `ordenarNumerosAscendente` ordena un array de números de forma ascendente.
* La función `ordenarNumerosDescendente` ordena un array de números de forma descendente.
* La función `busquedaBinaria` busca un elemento en un array de forma binaria.
* La función `generarNumeroAleatorio` genera un número aleatorio entre dos valores dados.
* La función `crearNuevoObjeto` crea un objeto con las propiedades de un objeto existente.
* La función `fusionarObjetos` fusiona dos objetos en uno solo.
* La función `obtenerMaximo` obtiene el elemento máximo de un array de números.
* La función `obtenerMinimo` obtiene el elemento mínimo de un array de números.
* La función `obtenerSuma` obtiene la suma de los elementos de un array de números.
* La función `obtenerProducto` obtiene el producto de los elementos de un array de números.
* La función `obtenerMedia` obtiene la media de los elementos de un array de números.
* La función `obtenerMediana` obtiene la mediana de los elementos de un array de números.
* La función `obtenerDesviacionEstandar` obtiene la desviación estándar de los elementos de un array de números.