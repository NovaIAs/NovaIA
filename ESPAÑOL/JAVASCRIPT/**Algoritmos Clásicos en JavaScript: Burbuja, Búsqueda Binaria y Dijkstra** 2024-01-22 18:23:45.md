```javascript
// Algoritmo de ordenación por burbuja en JavaScript

// Definimos la función de ordenación por burbuja
function burbuja(array) {
  // Obtenemos el tamaño del array
  let longitud = array.length;

  // Iteramos sobre el array de forma anidada
  for (let i = 0; i < longitud; i++) {
    for (let j = 0; j < longitud - i - 1; j++) {
      // Comparamos los elementos adyacentes
      if (array[j] > array[j + 1]) {
        // Si el elemento actual es mayor que el siguiente, los intercambiamos
        let temp = array[j];
        array[j] = array[j + 1];
        array[j + 1] = temp;
      }
    }
  }

  // Devolvemos el array ordenado
  return array;
}

// Ejemplo de uso
let array = [5, 3, 1, 2, 4];
console.log(burbuja(array)); // [1, 2, 3, 4, 5]


// Algoritmo de búsqueda binaria en JavaScript

// Definimos la función de búsqueda binaria
function busquedaBinaria(array, objetivo) {
  // Obtenemos el tamaño del array
  let longitud = array.length;

  // Definimos los índices inicial y final
  let inicio = 0;
  let fin = longitud - 1;

  // Mientras el índice inicial sea menor o igual al índice final
  while (inicio <= fin) {
    // Calculamos el índice medio
    let medio = Math.floor((inicio + fin) / 2);

    // Comparamos el objetivo con el elemento en el índice medio
    if (array[medio] === objetivo) {
      // Si son iguales, devolvemos el índice medio
      return medio;
    } else if (array[medio] < objetivo) {
      // Si el objetivo es mayor, actualizamos el índice inicial
      inicio = medio + 1;
    } else {
      // Si el objetivo es menor, actualizamos el índice final
      fin = medio - 1;
    }
  }

  // Si no encontramos el objetivo, devolvemos -1
  return -1;
}

// Ejemplo de uso
let array = [1, 3, 5, 7, 9, 11, 13, 15];
console.log(busquedaBinaria(array, 11)); // 4


// Algoritmo de Dijkstra para encontrar la ruta más corta en un grafo en JavaScript

// Definimos el grafo como un objeto de objetos
let grafo = {
  A: { B: 4, C: 2 },
  B: { A: 4, C: 3, D: 2 },
  C: { A: 2, B: 3, D: 4, E: 3 },
  D: { B: 2, C: 4, E: 1 },
  E: { C: 3, D: 1 }
};

// Definimos la función para encontrar la ruta más corta
function dijkstra(grafo, inicio, fin) {
  // Definimos las distancias desde el nodo inicial a todos los demás nodos
  let distancias = {};
  for (let nodo in grafo) {
    distancias[nodo] = Infinity;
  }
  distancias[inicio] = 0;

  // Definimos la cola de prioridad
  let cola = new PriorityQueue();
  cola.push(inicio, 0);

  // Mientras la cola no esté vacía
  while (!cola.isEmpty()) {
    // Obtenemos el nodo con la distancia más corta desde el nodo inicial
    let actual = cola.pop();

    // Si el nodo actual es el nodo final, hemos encontrado la ruta más corta
    if (actual === fin) {
      break;
    }

    // Iteramos sobre los vecinos del nodo actual
    for (let vecino in grafo[actual]) {
      // Calculamos la distancia desde el nodo inicial al vecino
      let distancia = distancias[actual] + grafo[actual][vecino];

      // Si la distancia es menor que la distancia actual del vecino, actualizamos la distancia del vecino
      if (distancia < distancias[vecino]) {
        distancias[vecino] = distancia;
        cola.push(vecino, distancia);
      }
    }
  }

  // Devolvemos la distancia desde el nodo inicial al nodo final
  return distancias[fin];
}

// Definimos la clase PriorityQueue
class PriorityQueue {
  constructor() {
    this.queue = [];
  }

  push(value, priority) {
    this.queue.push({ value, priority });
    this.queue.sort((a, b) => a.priority - b.priority);
  }

  pop() {
    return this.queue.shift().value;
  }

  isEmpty() {
    return this.queue.length === 0;
  }
}
``
// Ejemplo de uso
console.log(dijkstra(grafo, 'A', 'E')); // 6