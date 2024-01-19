```go
package main

import (
	"bufio"
	"fmt"
	"io"
	"os"
	"strconv"
	"strings"
)

type Node struct {
	value    int
	children []*Node
}

func main() {
	// Read the input from stdin
	reader := bufio.NewReader(os.Stdin)
	input, _ := reader.ReadString('\n')

	// Parse the input into a list of integers
	numbers := strings.Split(strings.TrimSpace(input), " ")
	numbersAsInts := make([]int, len(numbers))
	for i, number := range numbers {
		numbersAsInts[i], _ = strconv.Atoi(number)
	}

	// Create the root node of the tree
	root := &Node{value: numbersAsInts[0]}

	// Build the tree recursively
	buildTree(root, numbersAsInts[1:])

	// Print the tree in preorder traversal
	fmt.Println("Preorder traversal:")
	preorderTraversal(root)

	// Print the tree in inorder traversal
	fmt.Println("Inorder traversal:")
	inorderTraversal(root)

	// Print the tree in postorder traversal
	fmt.Println("Postorder traversal:")
	postorderTraversal(root)
}

// Build the tree recursively
func buildTree(root *Node, numbers []int) {
	if len(numbers) == 0 {
		return
	}

	// Create a new node for the left child
	leftChild := &Node{value: numbers[0]}

	// Create a new node for the right child
	rightChild := &Node{value: numbers[1]}

	// Add the left and right children to the current node
	root.children = append(root.children, leftChild, rightChild)

	// Recursively build the left and right subtrees
	buildTree(leftChild, numbers[2:])
	buildTree(rightChild, numbers[3:])
}

// Print the tree in preorder traversal
func preorderTraversal(root *Node) {
	if root == nil {
		return
	}

	// Print the value of the current node
	fmt.Print(root.value, " ")

	// Recursively print the left and right subtrees
	for _, child := range root.children {
		preorderTraversal(child)
	}
}

// Print the tree in inorder traversal
func inorderTraversal(root *Node) {
	if root == nil {
		return
	}

	// Recursively print the left subtree
	for _, child := range root.children[:len(root.children)-1] {
		inorderTraversal(child)
	}

	// Print the value of the current node
	fmt.Print(root.value, " ")

	// Recursively print the right subtree
	inorderTraversal(root.children[len(root.children)-1])
}

// Print the tree in postorder traversal
func postorderTraversal(root *Node) {
	if root == nil {
		return
	}

	// Recursively print the left and right subtrees
	for _, child := range root.children {
		postorderTraversal(child)
	}

	// Print the value of the current node
	fmt.Print(root.value, " ")
}
```

This code reads a list of integers from stdin and builds a binary tree from the input. The tree is then printed in preorder, inorder, and postorder traversals.

The `Node` struct represents a node in the tree. It has a `value` field, which stores the value of the node, and a `children` field, which stores a list of the node's children.

The `buildTree` function builds the tree recursively. It takes the root node of the tree and a list of integers as input. It creates a new node for the left and right children of the root node and adds them to the `children` field of the root node. It then recursively calls itself to build the left and right subtrees of the root node.

The `preorderTraversal`, `inorderTraversal`, and `postorderTraversal` functions print the tree in preorder, inorder, and postorder traversals, respectively. They do this by recursively visiting the left and right subtrees of the current node and printing the value of the current node.