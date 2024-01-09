```typescript
// MODULE 1: DATA STRUCTURES AND ALGORITHMS

// 1.1 Linked List
class Node {
  constructor(data) {
    this.data = data;
    this.next = null;
  }
}

class LinkedList {
  constructor() {
    this.head = null;
    this.size = 0;
  }

  // Add a node to the end of the list
  add(data) {
    const node = new Node(data);
    if (this.head === null) {
      this.head = node;
    } else {
      let current = this.head;
      while (current.next !== null) {
        current = current.next;
      }
      current.next = node;
    }
    this.size++;
  }

  // Remove a node from the list by its data
  remove(data) {
    if (this.head === null) {
      return;
    }
    if (this.head.data === data) {
      this.head = this.head.next;
      this.size--;
      return;
    }
    let current = this.head;
    let previous = null;
    while (current !== null && current.data !== data) {
      previous = current;
      current = current.next;
    }
    if (current !== null) {
      previous.next = current.next;
      this.size--;
    }
  }

  // Search for a node in the list by its data
  search(data) {
    if (this.head === null) {
      return null;
    }
    let current = this.head;
    while (current !== null && current.data !== data) {
      current = current.next;
    }
    return current;
  }

  // Print the list
  print() {
    let current = this.head;
    while (current !== null) {
      console.log(current.data);
      current = current.next;
    }
  }
}

// MODULE 2: SORTING ALGORITHMS

// 2.1 Bubble Sort
function bubbleSort(array) {
  for (let i = 0; i < array.length; i++) {
    for (let j = 0; j < array.length - i - 1; j++) {
      if (array[j] > array[j + 1]) {
        [array[j], array[j + 1]] = [array[j + 1], array[j]];
      }
    }
  }
}

// 2.2 Selection Sort
function selectionSort(array) {
  for (let i = 0; i < array.length; i++) {
    let minIndex = i;
    for (let j = i + 1; j < array.length; j++) {
      if (array[j] < array[minIndex]) {
        minIndex = j;
      }
    }
    [array[i], array[minIndex]] = [array[minIndex], array[i]];
  }
}

// 2.3 Insertion Sort
function insertionSort(array) {
  for (let i = 1; i < array.length; i++) {
    let current = array[i];
    let j = i - 1;
    while (j >= 0 && current < array[j]) {
      array[j + 1] = array[j];
      j--;
    }
    array[j + 1] = current;
  }
}

// MODULE 3: SEARCHING ALGORITHMS

// 3.1 Linear Search
function linearSearch(array, target) {
  for (let i = 0; i < array.length; i++) {
    if (array[i] === target) {
      return i;
    }
  }
  return -1;
}

// 3.2 Binary Search
function binarySearch(array, target) {
  let low = 0;
  let high = array.length - 1;
  while (low <= high) {
    let mid = Math.floor((low + high) / 2);
    if (array[mid] === target) {
      return mid;
    } else if (array[mid] < target) {
      low = mid + 1;
    } else {
      high = mid - 1;
    }
  }
  return -1;
}

// MODULE 4: GRAPH ALGORITHMS

// 4.1 Depth-First Search (DFS)
function depthFirstSearch(graph, startNode) {
  const visited = new Set();
  const stack = [startNode];
  while (stack.length > 0) {
    const current = stack.pop();
    if (!visited.has(current)) {
      visited.add(current);
      console.log(current);
      for (const neighbor of graph[current]) {
        if (!visited.has(neighbor)) {
          stack.push(neighbor);
        }
      }
    }
  }
}

// 4.2 Breadth-First Search (BFS)
function breadthFirstSearch(graph, startNode) {
  const visited = new Set();
  const queue = [startNode];
  while (queue.length > 0) {
    const current = queue.shift();
    if (!visited.has(current)) {
      visited.add(current);
      console.log(current);
      for (const neighbor of graph[current]) {
        if (!visited.has(neighbor)) {
          queue.push(neighbor);
        }
      }
    }
  }
}

// USAGE EXAMPLE
const linkedList = new LinkedList();
linkedList.add(1);
linkedList.add(2);
linkedList.add(3);
linkedList.add(4);
linkedList.add(5);
linkedList.print(); // Output: 1 2 3 4 5

console.log("Bubble Sort:");
const array1 = [5, 2, 8, 3, 1, 9, 4, 7, 6];
bubbleSort(array1);
console.log(array1); // Output: [1, 2, 3, 4, 5, 6, 7, 8, 9]

console.log("Selection Sort:");
const array2 = [5, 2, 8, 3, 1, 9, 4, 7, 6];
selectionSort(array2);
console.log(array2); // Output: [1, 2, 3, 4, 5, 6, 7, 8, 9]

console.log("Insertion Sort:");
const array3 = [5, 2, 8, 3, 1, 9, 4, 7, 6];
insertionSort(array3);
console.log(array3); // Output: [1, 2, 3, 4, 5, 6, 7, 8, 9]

console.log("Linear Search:");
const array4 = [5, 2, 8, 3, 1, 9, 4, 7, 6];
const target1 = 3;
const