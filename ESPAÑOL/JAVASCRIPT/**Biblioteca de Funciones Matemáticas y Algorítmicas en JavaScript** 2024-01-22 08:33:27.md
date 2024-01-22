```javascript
// Función para encontrar el máximo común divisor de dos números
function mcd(a, b) {
  if (b === 0) {
    return a;
  } else {
    return mcd(b, a % b);
  }
}

// Función para encontrar el mínimo común múltiplo de dos números
function mcm(a, b) {
  return (a * b) / mcd(a, b);
}

// Función para encontrar el factorial de un número
function factorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Función para encontrar la permutación de un conjunto de elementos
function permutacion(arr) {
  if (arr.length === 0) {
    return [[]];
  } else {
    let result = [];
    for (let i = 0; i < arr.length; i++) {
      let currElement = arr[i];
      let remainingElements = arr.slice(0, i).concat(arr.slice(i + 1));
      let permutacionesRestantes = permutacion(remainingElements);
      for (let j = 0; j < permutacionesRestantes.length; j++) {
        result.push([currElement].concat(permutacionesRestantes[j]));
      }
    }
    return result;
  }
}

// Función para encontrar la combinación de un conjunto de elementos
function combinacion(arr, k) {
  if (k === 0) {
    return [[]];
  } else if (arr.length === 0) {
    return [];
  } else {
    let result = [];
    let currElement = arr[0];
    let remainingElements = arr.slice(1);
    let combinacionesRestantes = combinacion(remainingElements, k - 1);
    for (let i = 0; i < combinacionesRestantes.length; i++) {
      result.push([currElement].concat(combinacionesRestantes[i]));
    }
    result = result.concat(combinacion(remainingElements, k));
    return result;
  }
}

// Función para encontrar el subconjunto de potencia de un conjunto de elementos
function subconjuntoPotencia(arr) {
  if (arr.length === 0) {
    return [[]];
  } else {
    let result = [];
    let currElement = arr[0];
    let remainingElements = arr.slice(1);
    let subconjuntosRestantes = subconjuntoPotencia(remainingElements);
    for (let i = 0; i < subconjuntosRestantes.length; i++) {
      result.push([currElement].concat(subconjuntosRestantes[i]));
    }
    result = result.concat(subconjuntoPotencia(remainingElements));
    return result;
  }
}

// Función para encontrar el árbol de expansión mínimo de un grafo
function mst(grafo) {
  // Inicializar el árbol de expansión mínimo
  let mst = {};
  // Inicializar el conjunto de vértices visitados
  let visited = {};
  // Inicializar el peso total del árbol de expansión mínimo
  let totalWeight = 0;
  // Empezar desde el primer vértice
  let currentVertex = grafo[0];
  // Marcar el primer vértice como visitado
  visited[currentVertex] = true;
  // Mientras haya vértices sin visitar
  while (Object.keys(visited).length < Object.keys(grafo).length) {
    // Encontrar el borde con el peso mínimo desde el vértice actual
    let minEdge = { weight: Infinity, from: null, to: null };
    for (let i = 0; i < grafo[currentVertex].length; i++) {
      let edge = grafo[currentVertex][i];
      if (!visited[edge.to] && edge.weight < minEdge.weight) {
        minEdge = edge;
      }
    }
    // Añadir el borde al árbol de expansión mínimo
    mst[minEdge.from] = minEdge.to;
    // Marcar el vértice de destino como visitado
    visited[minEdge.to] = true;
    // Añadir el peso del borde al peso total del árbol de expansión mínimo
    totalWeight += minEdge.weight;
    // Establecer el vértice actual como el vértice de destino
    currentVertex = minEdge.to;
  }
  // Devolver el árbol de expansión mínimo y el peso total del árbol de expansión mínimo
  return { mst, totalWeight };
}
```

Este código contiene una colección de funciones matemáticas y algorítmicas comunes en programación:

* **`mcd()` y `mcm()`:** Calcula el máximo común divisor y el mínimo común múltiplo de dos números.
* **`factorial()`:** Calcula el factorial de un número.
* **`permutacion()`:** Genera todas las permutaciones posibles de un conjunto de elementos.
* **`combinacion()`:** Genera todas las combinaciones posibles de un conjunto de elementos.
* **`subconjuntoPotencia()`:** Genera todos los subconjuntos posibles de un conjunto de elementos.
* **`mst()`:** Encuentra el árbol de expansión mínimo de un grafo.

El objetivo de incluir una variedad de funciones es mostrar la versatilidad y complejidad del lenguaje JavaScript, así como proporcionar herramientas útiles para resolver problemas de programación comunes.