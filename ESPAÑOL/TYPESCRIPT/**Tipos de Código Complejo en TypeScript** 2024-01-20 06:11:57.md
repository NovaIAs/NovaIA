**Función recursiva para calcular el factorial de un número:**

```typescript
function factorial(n: number): number {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}
```

Esta función calcula el factorial de un número utilizando la recursión. El factorial de un número es el producto de todos los números enteros positivos desde 1 hasta ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.

La función factorial se define de forma recursiva. Esto significa que se llama a sí misma para resolver el problema. En este caso, la función se llama a sí misma para calcular el factorial del número n - 1. Esto se hace hasta que el número n sea igual a 0. Cuando n es igual a 0, la función devuelve 1. Esto se debe a que el factorial de 0 es 1.

El código anterior es un ejemplo de un algoritmo recursivo. Los algoritmos recursivos son a menudo más fáciles de escribir y entender que los algoritmos iterativos. Sin embargo, los algoritmos recursivos pueden ser menos eficientes que los algoritmos iterativos en algunos casos.

**Clase para representar una lista enlazada:**

```typescript
class LinkedList {
  private head: Node | null;
  private tail: Node | null;

  constructor() {
    this.head = null;
    this.tail = null;
  }

  public add(value: any): void {
    const newNode = new Node(value);

    if (this.head === null) {
      this.head = newNode;
      this.tail = newNode;
    } else {
      this.tail.next = newNode;
      this.tail = newNode;
    }
  }

  public remove(value: any): boolean {
    let currentNode = this.head;
    let previousNode: Node | null = null;

    while (currentNode !== null) {
      if (currentNode.value === value) {
        if (previousNode === null) {
          this.head = currentNode.next;
        } else {
          previousNode.next = currentNode.next;
        }

        if (currentNode === this.tail) {
          this.tail = previousNode;
        }

        return true;
      }

      previousNode = currentNode;
      currentNode = currentNode.next;
    }

    return false;
  }

  public find(value: any): Node | null {
    let currentNode = this.head;

    while (currentNode !== null) {
      if (currentNode.value === value) {
        return currentNode;
      }

      currentNode = currentNode.next;
    }

    return null;
  }
}

class Node {
  public value: any;
  public next: Node | null;

  constructor(value: any) {
    this.value = value;
    this.next = null;
  }
}
```

Esta clase representa una lista enlazada. Una lista enlazada es una estructura de datos que almacena elementos en una secuencia lineal. Cada elemento de la lista enlazada se denomina nodo. Cada nodo tiene un valor y una referencia al siguiente nodo en la lista.

La clase LinkedList define tres métodos: `add()`, `remove()` y `find()`. El método `add()` añade un nuevo elemento al final de la lista. El método `remove()` elimina un elemento de la lista. El método `find()` busca un elemento en la lista.

El código anterior es un ejemplo de una implementación de una lista enlazada en TypeScript. Las listas enlazadas son a menudo más eficientes que las matrices para almacenar datos que cambian con frecuencia. Esto se debe a que las listas enlazadas no necesitan ser contiguas en memoria. Esto significa que los elementos de una lista enlazada pueden moverse a nuevas ubicaciones en memoria sin tener que copiar los datos.

**Algoritmo para ordenar una matriz de números:**

```typescript
function quickSort(array: number[]): number[] {
  if (array.length <= 1) {
    return array;
  }

  const pivot = array[0];
  const left: number[] = [];
  const right: number[] = [];

  for (let i = 1; i < array.length; i++) {
    if (array[i] < pivot) {
      left.push(array[i]);
    } else {
      right.push(array[i]);
    }
  }

  return quickSort(left).concat(pivot, quickSort(right));
}
```

Este algoritmo ordena una matriz de números utilizando el algoritmo de ordenación rápida. El algoritmo de ordenación rápida funciona dividiendo la matriz en dos partes: una parte que contiene los elementos menores que el pivote y otra parte que contiene los elementos mayores que el pivote. El pivote es el elemento que se encuentra en el medio de la matriz.

Una vez que la matriz se ha dividido en dos partes, el algoritmo se llama a sí mismo para ordenar cada una de las partes. Esto se hace hasta que todas las partes de la matriz estén ordenadas.

El código anterior es un ejemplo de una implementación del algoritmo de ordenación rápida en TypeScript. El algoritmo de ordenación rápida es uno de los algoritmos de ordenación más rápidos. Sin embargo, el algoritmo de ordenación rápida puede ser menos eficiente que otros algoritmos de ordenación para matrices grandes.

Estos son solo algunos ejemplos de código complejo en TypeScript. Hay muchos otros tipos de código complejo que se pueden escribir en TypeScript. TypeScript es un lenguaje de programación muy versátil que se puede utilizar para crear una amplia variedad de aplicaciones.