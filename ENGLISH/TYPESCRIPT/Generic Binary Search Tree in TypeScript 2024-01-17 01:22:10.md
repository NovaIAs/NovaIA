```typescript
// Define a generic function to compare two values of the same type
function compare<T>(a: T, b: T): number {
  if (a < b) {
    return -1;
  } else if (a > b) {
    return 1;
  } else {
    return 0;
  }
}

// Create a class to represent a Binary Search Tree (BST) node
class Node<T> {
  value: T;
  left: Node<T> | null;
  right: Node<T> | null;

  constructor(value: T) {
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

// Create a class to represent a Binary Search Tree (BST)
class BinarySearchTree<T> {
  root: Node<T> | null;

  constructor() {
    this.root = null;
  }

  // Insert a new value into the BST
  insert(value: T): void {
    const newNode = new Node(value);

    if (!this.root) {
      this.root = newNode;
      return;
    }

    this._insertNode(newNode, this.root);
  }

  // Helper function to insert a new node into the BST
  private _insertNode(newNode: Node<T>, currentNode: Node<T>): void {
    if (compare(newNode.value, currentNode.value) < 0) {
      if (currentNode.left) {
        this._insertNode(newNode, currentNode.left);
      } else {
        currentNode.left = newNode;
      }
    } else {
      if (currentNode.right) {
        this._insertNode(newNode, currentNode.right);
      } else {
        currentNode.right = newNode;
      }
    }
  }

  // Search for a value in the BST
  search(value: T): Node<T> | null {
    if (!this.root) {
      return null;
    }

    return this._searchNode(value, this.root);
  }

  // Helper function to search for a value in the BST
  private _searchNode(value: T, currentNode: Node<T>): Node<T> | null {
    if (compare(value, currentNode.value) === 0) {
      return currentNode;
    } else if (compare(value, currentNode.value) < 0) {
      if (currentNode.left) {
        return this._searchNode(value, currentNode.left);
      } else {
        return null;
      }
    } else {
      if (currentNode.right) {
        return this._searchNode(value, currentNode.right);
      } else {
        return null;
      }
    }
  }

  // Find the minimum value in the BST
  min(): Node<T> | null {
    if (!this.root) {
      return null;
    }

    return this._findMinNode(this.root);
  }

  // Helper function to find the minimum value in the BST
  private _findMinNode(currentNode: Node<T>): Node<T> {
    if (currentNode.left) {
      return this._findMinNode(currentNode.left);
    }

    return currentNode;
  }

  // Find the maximum value in the BST
  max(): Node<T> | null {
    if (!this.root) {
      return null;
    }

    return this._findMaxNode(this.root);
  }

  // Helper function to find the maximum value in the BST
  private _findMaxNode(currentNode: Node<T>): Node<T> {
    if (currentNode.right) {
      return this._findMaxNode(currentNode.right);
    }

    return currentNode;
  }

  // Delete a value from the BST
  delete(value: T): void {
    if (!this.root) {
      return;
    }

    this._deleteNode(value, this.root);
  }

  // Helper function to delete a value from the BST
  private _deleteNode(value: T, currentNode: Node<T>): void {
    if (compare(value, currentNode.value) < 0) {
      if (currentNode.left) {
        this._deleteNode(value, currentNode.left