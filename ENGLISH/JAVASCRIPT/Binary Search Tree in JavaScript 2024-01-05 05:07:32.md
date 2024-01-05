```javascript
// This code implements a binary search tree (BST) in JavaScript.
// A BST is a data structure that stores data in a hierarchical manner,
// allowing for efficient searching and retrieval of information.

// The Node class represents a single node in the BST.
// Each node has a value, a left child, and a right child.
class Node {
  constructor(value) {
    this.value = value;
    this.left = null;
    this.right = null;
  }
}

// The BinarySearchTree class represents the entire BST.
// It has a root node, which is the starting point for all searches and insertions.
class BinarySearchTree {
  constructor() {
    this.root = null;
  }

  // The insert method inserts a new value into the BST.
  // It recursively searches for the appropriate place to insert the new value,
  // based on the values of the existing nodes.
  insert(value) {
    if (this.root === null) {
      this.root = new Node(value);
      return;
    }

    this._insert(value, this.root);
  }

  // The _insert method is a helper method for the insert method.
  // It recursively searches for the appropriate place to insert the new value,
  // based on the values of the existing nodes.
  _insert(value, node) {
    if (value < node.value) {
      if (node.left === null) {
        node.left = new Node(value);
        return;
      } else {
        this._insert(value, node.left);
      }
    } else if (value > node.value) {
      if (node.right === null) {
        node.right = new Node(value);
        return;
      } else {
        this._insert(value, node.right);
      }
    } else {
      // If the value is equal to an existing value,
      // we don't insert it to avoid duplicate values.
      return;
    }
  }

  // The search method searches for a value in the BST.
  // It recursively searches through the tree, comparing the value to the values of the existing nodes.
  search(value) {
    if (this.root === null) {
      return false;
    }

    return this._search(value, this.root);
  }

  // The _search method is a helper method for the search method.
  // It recursively searches through the tree, comparing the value to the values of the existing nodes.
  _search(value, node) {
    if (value === node.value) {
      return true;
    } else if (value < node.value) {
      if (node.left === null) {
        return false;
      } else {
        return this._search(value, node.left);
      }
    } else if (value > node.value) {
      if (node.right === null) {
        return false;
      } else {
        return this._search(value, node.right);
      }
    }
  }

  // The delete method deletes a value from the BST.
  // It recursively searches for the value to be deleted and then removes it from the tree.
  delete(value) {
    if (this.root === null) {
      return;
    }

    this._delete(value, this.root, null);
  }

  // The _delete method is a helper method for the delete method.
  // It recursively searches for the value to be deleted and then removes it from the tree.
  _delete(value, node, parent) {
    if (value === node.value) {
      // If the node has no children, we can simply remove it.
      if (node.left === null && node.right === null) {
        if (parent === null) {
          this.root = null;
        } else if (parent.left === node) {
          parent.left = null;
        } else {
          parent.right = null;
        }
        return;
      }

      // If the node has one child, we can replace it with its child.
      else if (node.left === null) {
        if (parent === null) {
          this.root = node.right;
        } else if (parent.left === node) {
          parent.left = node.right;
        } else {
          parent.right = node.right;
        }
        return;
      } else if (node.right === null) {
        if (parent === null) {
          this.root = node.left;
        } else if (parent.left === node) {
          parent.left = node.left;
        } else {
          parent.right = node.left;
        }
        return;
      }

      // If the node has two children, we find the smallest value in the right subtree
      // and replace the node with that value.
      else {
        let successor = node.right;
        let successorParent