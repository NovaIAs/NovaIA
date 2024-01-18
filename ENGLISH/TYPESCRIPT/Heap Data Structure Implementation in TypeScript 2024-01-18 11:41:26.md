```typescript
// This is a TypeScript program that implements a heap data structure.
// A heap is a complete binary tree where each node is greater than or equal to its children.
// This program implements a max heap, which means that the root node is the largest element in the heap.

// The Heap class represents a heap data structure.
class Heap {
  // The constructor takes an array of elements as input and builds a heap from it.
  constructor(elements: number[]) {
    // The heap is represented as an array.
    this.heap = elements;

    // The size of the heap is the number of elements in the array.
    this.size = elements.length;

    // Build the heap from the input array.
    this.buildHeap();
  }

  // The buildHeap method builds a heap from an array of elements.
  private buildHeap() {
    // Start from the last non-leaf node and heapify all the subtrees.
    for (let i = Math.floor(this.size / 2 - 1); i >= 0; i--) {
      this.heapify(i);
    }
  }

  // The heapify method heapifies a subtree rooted at a given index.
  private heapify(index: number) {
    // Find the largest among the current node, its left child, and its right child.
    let largest = index;
    const left = 2 * index + 1;
    const right = 2 * index + 2;

    if (left < this.size && this.heap[left] > this.heap[largest]) {
      largest = left;
    }

    if (right < this.size && this.heap[right] > this.heap[largest]) {
      largest = right;
    }

    // If the largest is not the current node, swap the largest with the current node and heapify the subtree rooted at the largest.
    if (largest !== index) {
      [this.heap[index], this.heap[largest]] = [this.heap[largest], this.heap[index]];
      this.heapify(largest);
    }
  }

  // The insert method inserts a new element into the heap.
  insert(element: number) {
    // Increase the size of the heap.
    this.size++;

    // Add the new element to the end of the heap.
    this.heap[this.size - 1] = element;

    // Heapify the subtree rooted at the last node.
    this.heapify(this.size - 1);
  }

  // The extractMax method extracts the maximum element from the heap.
  extractMax(): number {
    // If the heap is empty, return null.
    if (this.size === 0) {
      return null;
    }

    // Swap the root node with the last node.
    [this.heap[0], this.heap[this.size - 1]] = [this.heap[this.size - 1], this.heap[0]];

    // Decrease the size of the heap.
    this.size--;

    // Heapify the subtree rooted at the new root node.
    this.heapify(0);

    // Return the maximum element.
    return this.heap[this.size];
  }

  // The printHeap method prints the heap in a level-order traversal.
  printHeap() {
    for (let i = 0; i < this.size; i++) {
      console.log(this.heap[i]);
    }
  }
}

// Create a heap from an array of elements.
const heap = new Heap([1, 3, 5, 7, 9, 2, 4, 6, 8]);

// Print the heap.
console.log("The heap is:");
heap.printHeap();

// Insert a new element into the heap.
heap.insert(10);

// Print the heap.
console.log("The heap after inserting 10 is:");
heap.printHeap();

// Extract the maximum element from the heap.
const max = heap.extractMax();

// Print the maximum element.
console.log("The maximum element is:", max);

// Print the heap.
console.log("The heap after extracting the maximum element is:");
heap.printHeap();
```