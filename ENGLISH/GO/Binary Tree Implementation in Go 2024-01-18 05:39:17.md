```go
package main

import (
	"bufio"
	"fmt"
	"log"
	"math"
	"math/rand"
	"os"
	"strconv"
	"strings"
	"time"
)

// Node represents a node in a binary tree.
type Node struct {
	key   int
	left  *Node
	right *Node
}

// Insert inserts a new node into the binary tree.
func Insert(root **Node, key int) {
	if *root == nil {
		*root = &Node{key: key}
		return
	}
	if key < (*root).key {
		Insert(&(*root).left, key)
	} else {
		Insert(&(*root).right, key)
	}
}

// Search searches for a key in the binary tree.
func Search(root *Node, key int) bool {
	if root == nil {
		return false
	}
	if root.key == key {
		return true
	}
	if key < root.key {
		return Search(root.left, key)
	} else {
		return Search(root.right, key)
	}
}

// Delete deletes a key from the binary tree.
func Delete(root **Node, key int) {
	if *root == nil {
		return
	}
	if key < (*root).key {
		Delete(&(*root).left, key)
	} else if key > (*root).key {
		Delete(&(*root).right, key)
	} else {
		if (*root).left == nil {
			*root = (*root).right
		} else if (*root).right == nil {
			*root = (*root).left
		} else {
			// Find the minimum node in the right subtree.
			min := (*root).right
			for min.left != nil {
				min = min.left
			}
			// Copy the minimum node's data to the root node.
			(*root).key = min.key
			// Delete the minimum node from the right subtree.
			Delete(&(*root).right, min.key)
		}
	}
}

// Print prints the binary tree in preorder traversal.
func Print(root *Node) {
	if root == nil {
		return
	}
	fmt.Print(root.key, " ")
	Print(root.left)
	Print(root.right)
}

// GetHeight returns the height of the binary tree.
func GetHeight(root *Node) int {
	if root == nil {
		return 0
	}
	leftHeight := GetHeight(root.left)
	rightHeight := GetHeight(root.right)
	return 1 + Max(leftHeight, rightHeight)
}

// Max returns the maximum of two integers.
func Max(a, b int) int {
	if a > b {
		return a
	}
	return b
}

// IsBalanced checks if the binary tree is balanced.
func IsBalanced(root *Node) bool {
	if root == nil {
		return true
	}
	leftHeight := GetHeight(root.left)
	rightHeight := GetHeight(root.right)
	return math.Abs(float64(leftHeight-rightHeight)) <= 1 && IsBalanced(root.left) && IsBalanced(root.right)
}

// GetDiameter returns the diameter of the binary tree.
func GetDiameter(root *Node) int {
	if root == nil {
		return 0
	}
	leftDiameter := GetDiameter(root.left)
	rightDiameter := GetDiameter(root.right)
	return Max(leftDiameter, rightDiameter) + 1
}

// GetNodeCount returns the number of nodes in the binary tree.
func GetNodeCount(root *Node) int {
	if root == nil {
		return 0
	}
	return 1 + GetNodeCount(root.left) + GetNodeCount(root.right)
}

// GetLeafCount returns the number of leaf nodes in the binary tree.
func GetLeafCount(root *Node) int {
	if root == nil {
		return 0
	}
	if root.left == nil && root.right == nil {
		return 1
	}
	return GetLeafCount(root.left) + GetLeafCount(root.right)
}

// GetLevelOrderTraversal returns the level order traversal of the binary tree.
func GetLevelOrderTraversal(root *Node) [][]int {
	result := make([][]int, 0)
	queue := make([]*Node, 0)
	queue = append(queue, root)
	for len(queue) > 0 {
		level := make([]int, 0)
		for i := 0; i < len(queue); i++ {
			node := queue[i]
			level = append(level, node.key)
			if node.left != nil {
				queue = append(queue, node.left)
			}