```typescript
// This code implements a binary tree in TypeScript.

// A binary tree node.
class BinaryTreeNode<T> {
  public value: T;
  public left: BinaryTreeNode<T> | null;
  public right: BinaryTreeNode<T> | null;

  constructor(value: T) {
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

// A binary tree.
class BinaryTree<T> {
  public root: BinaryTreeNode<T> | null;

  constructor() {
    this.root = null;
  }

  // Insert a value into the binary tree.
  public insert(value: T): void {
    const newNode = new BinaryTreeNode(value);

    if (this.root === null) {
      this.root = newNode;
      return;
    }

    this._insertNode(newNode, this.root);
  }

  // Insert a new node into the binary tree.
  private _insertNode(newNode: BinaryTreeNode<T>, currentNode: BinaryTreeNode<T>): void {
    if (newNode.value < currentNode.value) {
      if (currentNode.left === null) {
        currentNode.left = newNode;
      } else {
        this._insertNode(newNode, currentNode.left);
      }
    } else {
      if (currentNode.right === null) {
        currentNode.right = newNode;
      } else {
        this._insertNode(newNode, currentNode.right);
      }
    }
  }

  // Find a value in the binary tree.
  public find(value: T): BinaryTreeNode<T> | null {
    return this._findNode(value, this.root);
  }

  // Find a node in the binary tree.
  private _findNode(value: T, currentNode: BinaryTreeNode<T> | null): BinaryTreeNode<T> | null {
    if (currentNode === null) {
      return null;
    }

    if (value < currentNode.value) {
      return this._findNode(value, currentNode.left);
    } else if (value > currentNode.value) {
      return this._findNode(value, currentNode.right);
    } else {
      return currentNode;
    }
  }

  // Delete a value from the binary tree.
  public delete(value: T): void {
    this.root = this._deleteNode(value, this.root);
  }

  // Delete a node from the binary tree.
  private _deleteNode(value: T, currentNode: BinaryTreeNode<T> | null): BinaryTreeNode<T> | null {
    if (currentNode === null) {
      return null;
    }

    if (value < currentNode.value) {
      currentNode.left = this._deleteNode(value, currentNode.left);
    } else if (value > currentNode.value) {
      currentNode.right = this._deleteNode(value, currentNode.right);
    } else {
      if (currentNode.left === null) {
        return currentNode.right;
      } else if (currentNode.right === null) {
        return currentNode.left;
      }

      const minValueNode = this._findMinValueNode(currentNode.right);
      currentNode.value = minValueNode.value;
      currentNode.right = this._deleteNode(minValueNode.value, currentNode.right);
    }

    return currentNode;
  }

  // Find the minimum value node in the binary tree.
  private _findMinValueNode(