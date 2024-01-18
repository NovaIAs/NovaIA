```go
package main

import (
	"fmt"
	"math/rand"
	"time"
)

// Node represents a node in a binary tree.
type Node struct {
	value int
	left  *Node
	right *Node
}

// NewNode creates a new node with the given value.
func NewNode(value int) *Node {
	return &Node{value: value}
}

// Insert inserts the given value into the binary tree.
func (n *Node) Insert(value int) {
	if value < n.value {
		if n.left == nil {
			n.left = NewNode(value)
		} else {
			n.left.Insert(value)
		}
	} else {
		if n.right == nil {
			n.right = NewNode(value)
		} else {
			n.right.Insert(value)
		}
	}
}

// Search searches for the given value in the binary tree.
func (n *Node) Search(value int) bool {
	if n == nil {
		return false
	}
	if value == n.value {
		return true
	} else if value < n.value {
		return n.left.Search(value)
	} else {
		return n.right.Search(value)
	}
}

// PrintInOrder prints the values of the binary tree in order.
func (n *Node) PrintInOrder() {
	if n.left != nil {
		n.left.PrintInOrder()
	}
	fmt.Print(n.value, " ")
	if n.right != nil {
		n.right.PrintInOrder()
	}
}

// main creates a binary tree and inserts some values into it.
func main() {
	rand.Seed(time.Now().UnixNano())

	// Create a binary tree.
	tree := NewNode(50)

	// Insert some values into the binary tree.
	for i := 0; i < 10; i++ {
		tree.Insert(rand.Intn(100))
	}

	// Print the values of the binary tree in order.
	tree.PrintInOrder()
}
```

**Explanation:**

This code creates a binary tree and inserts some values into it. The binary tree is a data structure that stores data in a hierarchical manner. Each node in the tree can have at most two child nodes, one on the left and one on the right.

The `Node` struct represents a node in the binary tree. It has three fields: `value`, `left`, and `right`. The `value` field stores the value of the node, the `left` field stores a pointer to the left child node, and the `right` field stores a pointer to the right child node.

The `NewNode` function creates a new node with the given value.

The `Insert` method inserts the given value into the binary tree. It first checks if the value is less than the value of the current node. If it is, it checks if the left child node of the current node is nil. If it is, it creates a new node with the given value and sets it as the left child node of the current node. Otherwise, it calls the `Insert` method on the left child node of the current node.

If the value is greater than or equal to the value of the current node, it checks if the right child node of the current node is nil. If it is, it creates a new node with the given value and sets it as the right child node of the current node. Otherwise, it calls the `Insert` method on the right child node of the current node.

The `Search` method searches for the given value in the binary tree. It first checks if the current node is nil. If it is, it returns false. It then checks if the value is equal to the value of the current node. If it is, it returns true. Otherwise, it checks if the value is less than the value of the current node. If it is, it calls the `Search` method on the left child node of the current node. Otherwise, it calls the `Search` method on the right child node of the current node.

The `PrintInOrder` method prints the values of the binary tree in order. It first calls the `PrintInOrder` method on the left child node of the current node. It then prints the value of the current node. It then calls the `PrintInOrder` method on the right child node of the current node.

The `main` function creates a binary tree and inserts some values into it. It then prints the values of the binary tree in order.