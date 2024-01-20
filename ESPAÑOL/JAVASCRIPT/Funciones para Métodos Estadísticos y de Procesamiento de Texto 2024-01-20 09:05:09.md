```javascript
// Funciones

// Función para calcular el factorial de un número
function factorial(n) {
  if (n === 0) {
    return 1;
  }
  return n * factorial(n - 1);
}

// Función para calcular el número de combinaciones de k elementos tomados de un conjunto de n elementos
function combinaciones(n, k) {
  return factorial(n) / (factorial(k) * factorial(n - k));
}

// Función para generar un número aleatorio entre dos números dados
function numeroAleatorio(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Función para generar una lista de números aleatorios únicos entre dos números dados
function listaNumerosAleatoriosUnicos(min, max, cantidad) {
  const numeros = [];
  while (numeros.length < cantidad) {
    const numero = numeroAleatorio(min, max);
    if (!numeros.includes(numero)) {
      numeros.push(numero);
    }
  }
  return numeros;
}

// Función para comprobar si un número es primo
function esPrimo(n) {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
}

// Función para generar una lista de números primos entre dos números dados
function listaNumerosPrimos(min, max) {
  const numeros = [];
  for (let i = min; i <= max; i++) {
    if (esPrimo(i)) {
      numeros.push(i);
    }
  }
  return numeros;
}

// Función para comprobar si un string es un palíndromo
function esPalindromo(string) {
  const stringSinEspacios = string.replace(/\s/g, "");
  const stringReversa = stringSinEspacios.split("").reverse().join("");
  return stringSinEspacios === stringReversa;
}

// Función para generar una lista de palabras palíndromas a partir de un array de palabras
function listaPalabrasPalindromas(palabras) {
  const palindromos = [];
  for (const palabra of palabras) {
    if (esPalindromo(palabra)) {
      palindromos.push(palabra);
    }
  }
  return palindromos;
}

// Función para crear un objeto con las letras del alfabeto como claves y sus posiciones en el alfabeto como valores
function crearObjetoAlfabeto() {
  const alfabeto = {};
  for (let i = 0; i < 26; i++) {
    const letra = String.fromCharCode(i + 97);
    alfabeto[letra] = i;
  }
  return alfabeto;
}

// Función para calcular la distancia de Levenshtein entre dos strings
function distanciaLevenshtein(string1, string2) {
  const m = string1.length;
  const n = string2.length;
  const matrizLevenshtein = new Array(m + 1).fill(0).map(() => new Array(n + 1).fill(0));

  for (let i = 0; i <= m; i++) {
    matrizLevenshtein[i][0] = i;
  }
  for (let j = 0; j <= n; j++) {
    matrizLevenshtein[0][j] = j;
  }

  for (let i = 1; i <= m; i++) {
    for (let j = 1; j <= n; j++) {
      const costoInsercion = matrizLevenshtein[i][j - 1] + 1;
      const costoEliminacion = matrizLevenshtein[i - 1][j] + 1;
      const costoSustitucion = matrizLevenshtein[i - 1][j - 1] + (string1[i - 1] === string2[j - 1] ? 0 : 1);

      matrizLevenshtein[i][j] = Math.min(costoInsercion, costoEliminacion, costoSustitucion);
    }
  }

  return matrizLevenshtein[m][n];
}

// Función para ordenar un array de objetos por una clave determinada
function ordenarArrayDeObjetosPorClave(array, clave) {
  return array.sort((a, b) => a[clave] - b[clave]);
}

// Función para agrupar los elementos de un array en objetos en función de una clave determinada
function agruparArrayDeObjetosPorClave(array, clave) {
  const grupos = {};
  for (const elemento of array) {
    const grupo = elemento[clave];
    if (!grupos[grupo]) {
      grupos[grupo] = [];
    }
    grupos[grupo].push(elemento);
  }
  return grupos;
}

// Función para crear un gráfico a partir de una matriz de adyacencia
function crearGrafo(matrizAdyacencia) {
  const grafo = {};
  for (let i = 0; i < matrizAdyacencia.length; i++) {
    const nodo = i;
    const vecinos = [];
    for (let j = 0; j < matrizAdyacencia[i].length; j++) {
      if (matrizAdyacencia[i][j] > 0) {
        vecinos.push(j);
      }
    }
    grafo[nodo] = vecinos;
  }
  return grafo;
}

// Función para realizar una búsqueda en profundidad en un grafo
function busquedaEnProfundidad(grafo, nodoInicial) {
  const visitados = new Set();
  const pila = [nodoInicial];

  while