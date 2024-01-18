```portugal

#include <stdio.h>
#include <stdlib.h>

// Define the maximum number of elements in the array
#define MAX_SIZE 100

// Define the structure of a node in the binary tree
typedef struct node {
  int data;
  struct node *left;
  struct node *right;
} node;

// Function to create a new node in the binary tree
node *create_node(int data) {
  node *new_node = (node *)malloc(sizeof(node));
  new_node->data = data;
  new_node->left = NULL;
  new_node->right = NULL;
  return new_node;
}

// Function to insert a node into the binary tree
void insert_node(node **root, int data) {
  if (*root == NULL) {
    *root = create_node(data);
  } else if (data < (*root)->data) {
    insert_node(&(*root)->left, data);
  } else {
    insert_node(&(*root)->right, data);
  }
}

// Function to search for a node in the binary tree
node *search_node(node *root, int data) {
  if (root == NULL) {
    return NULL;
  } else if (root->data == data) {
    return root;
  } else if (data < root->data) {
    return search_node(root->left, data);
  } else {
    return search_node(root->right, data);
  }
}

// Function to delete a node from the binary tree
void delete_node(node **root, int data) {
  if (*root == NULL) {
    return;
  } else if (data < (*root)->data) {
    delete_node(&(*root)->left, data);
  } else if (data > (*root)->data) {
    delete_node(&(*root)->right, data);
  } else {
    // Found the node to be deleted
    if ((*root)->left == NULL) {
      // The node has no left child
      node *temp = *root;
      *root = (*root)->right;
      free(temp);
    } else if ((*root)->right == NULL) {
      // The node has no right child
      node *temp = *root;
      *root = (*root)->left;
      free(temp);
    } else {
      // The node has both left and right children
      // Find the smallest node in the right subtree
      node *temp = (*root)->right;
      while (temp->left != NULL) {
        temp = temp->left;
      }
      // Copy the data from the smallest node to the node to be deleted
      (*root)->data = temp->data;
      // Delete the smallest node
      delete_node(&(*root)->right, temp->data);
    }
  }
}

// Function to print the binary tree in inorder traversal
void inorder_traversal(node *root) {
  if (root != NULL) {
    inorder_traversal(root->left);
    printf("%d ", root->data);
    inorder_traversal(root->right);
  }
}

// Function to print the binary tree in preorder traversal
void preorder_traversal(node *root) {
  if (root != NULL) {
    printf("%d ", root->data);
    preorder_traversal(root->left);
    preorder_traversal(root->right);
  }
}

// Function to print the binary tree in postorder traversal
void postorder_traversal(node *root) {
  if (root != NULL) {
    postorder_traversal(root->left);
    postorder_traversal(root->right);
    printf("%d ", root->data);
  }
}

// Main function
int main() {
  // Create the root node of the binary tree
  node *root = NULL;

  // Insert some data into the binary tree
  insert_node(&root, 50);
  insert_node(&root, 30);
  insert_node(&root, 70);
  insert_node(&root, 20);
  insert_node(&root, 40);
  insert_node(&root, 60);
  insert_node(&root, 80);

  // Search for a node in the binary tree
  node *found_node = search_node(root, 40);
  if (found_node != NULL) {
    printf("Node with data %d found.\n", found_node->data);
  } else {
    printf("Node not found.\n");
  }

  // Delete a node from the binary tree
  delete_node(&root, 20);

  // Print the binary tree in inorder traversal
  printf("Inorder traversal: ");
  inorder_traversal(root);
  printf("\n");

  // Print the binary tree in preorder traversal
  printf("Preorder traversal: ");
  preorder_traversal(root);
  printf("\n");

  // Print the binary tree in postorder traversal
  printf("Postorder traversal: ");
  postorder_traversal(root);
  printf("\n");

  return 0;
}

```

This code implements a binary tree in PORTUGOL.

The binary tree is a data structure that consists of nodes, each one containing a piece of data and two pointers to other nodes, called left and right.

The code defines a structure called `node` that represents a node in the binary tree.
It has three fields: `data`, `left`, and `right`.
The `data` field stores the data associated with the node, while the `left` and `right` fields store pointers to the left and right child nodes of the current node, respectively.

The code then defines a number of functions to operate on the binary tree.
These functions include:

* `create_node`: Creates a new node in the binary tree and returns a pointer to it.
* `insert_node`: Inserts a new node into the binary tree.
* `search_node`: Searches for a node in the binary tree and returns a pointer to it if it is found.
* `delete_node`: Deletes a node from the binary tree.
* `inorder_traversal`: Prints the binary tree in inorder traversal.
* `preorder_traversal`: Prints the binary tree in preorder traversal.
* `postorder_traversal`: Prints the binary tree in postorder traversal.

The main function creates a binary tree and inserts some data into it.
It then searches for a node in the binary tree, deletes a node from the binary tree, and prints the binary tree in inorder, preorder, and postorder traversals.