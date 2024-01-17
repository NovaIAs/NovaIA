```objective-c
#import <Foundation/Foundation.h>

// Define a constant for the maximum number of nodes in the binary tree
#define MAX_NODES 100

// Declare the node structure for the binary tree
struct Node {
    int data;
    struct Node *left;
    struct Node *right;
};

// Declare a pointer to the root node of the binary tree
struct Node *root = NULL;

// Function to insert a node into the binary tree
void insertNode(int data) {
    // Create a new node
    struct Node *newNode = (struct Node *)malloc(sizeof(struct Node));
    newNode->data = data;
    newNode->left = NULL;
    newNode->right = NULL;

    // If the tree is empty, set the new node as the root node
    if (root == NULL) {
        root = newNode;
        return;
    }

    // Otherwise, find the correct position for the new node in the tree
    struct Node *currentNode = root;
    while (1) {
        if (data < currentNode->data) {
            // If the new node's data is less than the current node's data, go left
            if (currentNode->left == NULL) {
                // If there is no left child, insert the new node as the left child
                currentNode->left = newNode;
                return;
            } else {
                // Otherwise, move to the left child
                currentNode = currentNode->left;
            }
        } else {
            // If the new node's data is greater than or equal to the current node's data, go right
            if (currentNode->right == NULL) {
                // If there is no right child, insert the new node as the right child
                currentNode->right = newNode;
                return;
            } else {
                // Otherwise, move to the right child
                currentNode = currentNode->right;
            }
        }
    }
}

// Function to search for a node in the binary tree
struct Node *searchNode(int data) {
    // Start at the root node
    struct Node *currentNode = root;

    // While the current node is not NULL and the data is not found
    while (currentNode != NULL && currentNode->data != data) {
        // If the data is less than the current node's data, go left
        if (data < currentNode->data) {
            currentNode = currentNode->left;
        } else {
            // Otherwise, go right
            currentNode = currentNode->right;
        }
    }

    // Return the current node, which is either the node containing the data or NULL if the data is not found
    return currentNode;
}

// Function to delete a node from the binary tree
void deleteNode(int data) {
    // Find the node to be deleted
    struct Node *nodeToDelete = searchNode(data);

    // If the node to be deleted is NULL, return
    if (nodeToDelete == NULL) {
        return;
    }

    // If the node to be deleted has no children, simply delete it
    if (nodeToDelete->left == NULL && nodeToDelete->right == NULL) {
        // If the node to be deleted is the root node, set the root node to NULL
        if (nodeToDelete == root) {
            root = NULL;
            return;
        }

        // Otherwise, find the parent of the node to be deleted and set the appropriate child pointer to NULL
        struct Node *parentNode = findParent(nodeToDelete);
        if (parentNode->left == nodeToDelete) {
            parentNode->left = NULL;
        } else {
            parentNode->right = NULL;
        }

        // Free the memory allocated for the node to be deleted
        free(nodeToDelete);
        return;
    }

    // If the node to be deleted has one child, replace the node to be deleted with its child
    if (nodeToDelete->left == NULL) {
        // If the node to be deleted is the root node, set the root node to the right child
        if (nodeToDelete == root) {
            root = nodeToDelete->right;
            return;
        }

        // Otherwise, find the parent of the node to be deleted and set the appropriate child pointer to the right child
        struct Node *parentNode = findParent(nodeToDelete);
        if (parentNode->left == nodeToDelete) {
            parentNode->left = nodeToDelete->right;
        } else {
            parentNode->right = nodeToDelete->right;
        }

        // Free the memory allocated for the node to be deleted
        free(nodeToDelete);
        return;
    } else if (nodeToDelete->right == NULL) {
        // If the node to be deleted is the root node, set the root node to the left child
        if (nodeToDelete == root) {
            root = nodeToDelete->left;
            return;
        }

        // Otherwise, find the parent of the node to be deleted and set the appropriate child