```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type Node struct {
    value int
    next  *Node
}

type LinkedList struct {
    head *Node
    tail *Node
    size int
}

func main() {
    // Create a new linked list.
    list := &LinkedList{}

    // Generate a random seed for the random number generator.
    rand.Seed(time.Now().UnixNano())

    // Insert 10 random numbers into the linked list.
    for i := 0; i < 10; i++ {
        value := rand.Intn(100)
        list.Insert(value)
    }

    // Print the linked list.
    fmt.Println("Linked list:")
    list.Print()

    // Reverse the linked list.
    list.Reverse()

    // Print the reversed linked list.
    fmt.Println("Reversed linked list:")
    list.Print()

    // Search for a value in the linked list.
    value := 50
    result, index := list.Search(value)
    if result {
        fmt.Printf("Value %d found at index %d\n", value, index)
    } else {
        fmt.Printf("Value %d not found\n", value)
    }

    // Delete a value from the linked list.
    value = 25
    list.Delete(value)

    // Print the linked list after deletion.
    fmt.Println("Linked list after deletion:")
    list.Print()
}

// Insert a value into the linked list.
func (list *LinkedList) Insert(value int) {
    // Create a new node with the given value.
    node := &Node{value: value}

    // If the linked list is empty, set the head and tail to the new node.
    if list.size == 0 {
        list.head = node
        list.tail = node
    } else {
        // Append the new node to the end of the linked list.
        list.tail.next = node
        list.tail = node
    }

    // Increment the size of the linked list.
    list.size++
}

// Print the linked list.
func (list *LinkedList) Print() {
    // Create a temporary node to iterate through the linked list.
    temp := list.head

    // Print each node's value.
    for temp != nil {
        fmt.Printf("%d ", temp.value)
        temp = temp.next
    }

    fmt.Println()
}

// Reverse the linked list.
func (list *LinkedList) Reverse() {
    // Create three temporary nodes to manipulate the linked list.
    var prev, current, next *Node

    // Set the previous node to nil.
    prev = nil

    // Set the current node to the head of the linked list.
    current = list.head

    // Iterate through the linked list.
    for current != nil {
        // Set the next node to the current node's next node.
        next = current.next

        // Set the current node's next node to the previous node.
        current.next = prev

        // Set the previous node to the current node.
        prev = current

        // Set the current node to the next node.
        current = next
    }

    // Set the head of the linked list to the previous node.
    list.head = prev
}

// Search for a value in the linked list.
func (list *LinkedList) Search(value int) (bool, int) {
    // Create a temporary node to iterate through the linked list.
    temp := list.head

    // Keep track of the index of the current node.
    index := 0

    // Iterate through the linked list.
    for temp != nil {
        // Check if the current node's value is equal to the given value.
        if temp.value == value {
            // Return true and the index of the node.
            return true, index
        }

        // Increment the index of the current node.
        index++

        // Move to the next node.
        temp = temp.next
    }

    // Return false and -1 if the value was not found.
    return false, -1
}

// Delete a value from the linked list.
func (list *LinkedList) Delete(value int) {
    // Create a temporary node to iterate through the linked list.
    temp := list.head

    // Keep track of the previous node.
    var prev *Node

    // Iterate through the linked list.
    for temp != nil {
        // Check if the current node's value is equal to the given value.
        if temp.value == value {
            // If the current node is the head of the linked list.
            if temp == list.head {
                // Set the head of the linked list to the next node.
                list.head = temp.next
            } else {
                // Set the previous node's next node to the current node's next node.
                prev.next = temp.next
            }

            // If the current node is the tail of the linked list.
            if temp == list.tail {
                // Set the tail of the linked list to the previous node.
                list.tail = prev
            }

            // Decrement the size of the linked list.
            list.size--

            // Break out of the loop.
            break
        }

        // Set the previous node to the current node.
        prev = temp

        // Move to the next node.
        temp = temp.next