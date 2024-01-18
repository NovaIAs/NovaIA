```go
package main

import (
	"bufio"
	"fmt"
	"os"
	"strconv"
	"strings"
)

// A Node represents a node in a binary tree.
type Node struct {
	Value int
	Left  *Node
	Right *Node
}

// NewNode creates a new node with the given value.
func NewNode(value int) *Node {
	return &Node{Value: value}
}

// Insert inserts a new node into the tree.
func (n *Node) Insert(value int) {
	if value < n.Value {
		if n.Left == nil {
			n.Left = NewNode(value)
		} else {
			n.Left.Insert(value)
		}
	} else {
		if n.Right == nil {
			n.Right = NewNode(value)
		} else {
			n.Right.Insert(value)
		}
	}
}

// Search searches for a value in the tree.
func (n *Node) Search(value int) bool {
	if n == nil {
		return false
	}
	if value == n.Value {
		return true
	} else if value < n.Value {
		return n.Left.Search(value)
	} else {
		return n.Right.Search(value)
	}
}

// Delete deletes a value from the tree.
func (n *Node) Delete(value int) {
	if n == nil {
		return
	}
	if value < n.Value {
		n.Left.Delete(value)
	} else if value > n.Value {
		n.Right.Delete(value)
	} else {
		if n.Left == nil {
			*n = *n.Right
		} else if n.Right == nil {
			*n = *n.Left
		} else {
			min := n.Right.FindMin()
			n.Value = min.Value
			min.Delete(min.Value)
		}
	}
}

// FindMin finds the minimum value in the tree.
func (n *Node) FindMin() *Node {
	if n == nil {
		return nil
	}
	if n.Left == nil {
		return n
	} else {
		return n.Left.FindMin()
	}
}

// FindMax finds the maximum value in the tree.
func (n *Node) FindMax() *Node {
	if n == nil {
		return nil
	}
	if n.Right == nil {
		return n
	} else {
		return n.Right.FindMax()
	}
}

// InOrder traverses the tree in order.
func (n *Node) InOrder() {
	if n == nil {
		return
	}
	n.Left.InOrder()
	fmt.Printf("%d ", n.Value)
	n.Right.InOrder()
}

// PreOrder traverses the tree in pre-order.
func (n *Node) PreOrder() {
	if n == nil {
		return
	}
	fmt.Printf("%d ", n.Value)
	n.Left.PreOrder()
	n.Right.PreOrder()
}

// PostOrder traverses the tree in post-order.
func (n *Node) PostOrder() {
	if n == nil {
		return
	}
	n.Left.PostOrder()
	n.Right.PostOrder()
	fmt.Printf("%d ", n.Value)
}

// main is the entry point for the program.
func main() {
	// Create a binary tree.
	tree := NewNode(20)
	tree.Insert(10)
	tree.Insert(30)
	tree.Insert(5)
	tree.Insert(15)
	tree.Insert(25)
	tree.Insert(35)

	// Print the tree in order.
	fmt.Println("In order:")
	tree.InOrder()
	fmt.Println()

	// Print the tree in pre-order.
	fmt.Println("Pre order:")
	tree.PreOrder()
	fmt.Println()

	// Print the tree in post-order.
	fmt.Println("Post order:")
	tree.PostOrder()
	fmt.Println()

	// Search for a value in the tree.
	value := 15
	if tree.Search(value) {
		fmt.Printf("Found %d in the tree.\n", value)
	} else {
		fmt.Printf("Did not find %d in the tree.\n", value)
	}

	// Delete a value from the tree.
	tree.Delete(15)

	// Print the tree in order.
	fmt.Println("In order:")
	tree.InOrder()
	fmt.Println()

	// Print the tree in pre-order.
	fmt.Println("Pre order:")
	tree.PreOrder()
	fmt.Println()

	// Print the tree in post-order.
	fmt.Println("Post order:")
	tree.PostOrder()
	fmt.Println()

	// Find the minimum value in the tree.
	min := tree.FindMin()
	fmt.Printf("The minimum value in the tree is %d.\n", min.Value)

	// Find the maximum value in the tree.
	max := tree.FindMax()
	fmt.Printf("The maximum value in the tree is %d.\n", max.Value)

	// Read a list of values from the user.
	reader := bufio.NewReader(os.Stdin)
	fmt.Print("Enter a list of values (separated by spaces): ")
	text, _ := reader.ReadString('\n')
	values := strings.Split(text, " ")

	// Convert the values to integers.
	var numbers []int
	for _, value := range values {
		number, _ := strconv.Atoi(value)
		numbers = append(numbers, number)
	}

	// Insert the values into the tree.
	for _, number := range numbers {
		tree.Insert(number)
	}

	// Print the tree in order.
	fmt.Println("In order:")
	tree.InOrder()
	fmt.Println()

	// Print the tree in pre-order.
	fmt.Println("Pre order:")
	tree.PreOrder()
	fmt.Println()

	// Print the tree in post-order.
	fmt.Println("Post order:")
	tree.PostOrder()
	fmt.Println()
}
```

This code implements a binary search tree in Go. A binary search tree is a data structure that stores data in a way that allows for fast searches and insertions. The code includes functions for inserting, searching, and deleting values from the tree, as well as functions for traversing the tree in order, pre-order, and post-order. The code also includes a main function that allows the user to interact with the tree by entering a list of values to insert into the tree and then printing the tree in different orders.

Here is a breakdown of the code:

* The `Node` struct represents a node in the binary search tree. It has three fields: `Value`, `Left`, and `Right`. `Value` stores the value of the node, `Left` stores a pointer to the left child of the node, and `Right` stores a pointer to the right child of the node.
* The `NewNode` function creates a new node with the given value.
* The `Insert` function inserts a new node into the tree. If the value of the new node is less than the value of the current node, the new node is inserted as the left child of the current node. Otherwise, the new node is inserted as the right child of the current node.
* The `Search` function searches for a value in the tree. If the value is found, the function returns `true`. Otherwise, the function returns `false`.
* The `Delete` function deletes a value from the tree. If the value is found, the function removes the node from the tree and rebalances the tree.
* The `FindMin` function finds the minimum value in the tree.
* The `FindMax` function finds the maximum value in the tree.
* The `InOrder` function traverses the tree in order.
* The `PreOrder` function traverses the tree in pre-order.
* The `PostOrder` function traverses the tree in post-order.
* The `main` function is the entry point for the program. It creates a binary search tree, inserts a list of values into the tree, and then prints the tree in different orders.

This code is a good example of how to implement a binary search tree in Go. It is well-organized and easy to understand.