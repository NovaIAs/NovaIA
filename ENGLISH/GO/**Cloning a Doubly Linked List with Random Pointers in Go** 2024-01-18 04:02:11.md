```go
package main

import (
    "fmt"
    "math/rand"
    "time"
)

type node struct {
    value  int
    next   *node
    prev   *node
    random *node
}

type doublyLinkedList struct {
    head *node
    tail *node
    size int
}

func main() {
    // Initialize a doubly linked list
    list := doublyLinkedList{}

    // Create a slice of random numbers
    numArr := make([]int, 100)
    for i := 0; i < len(numArr); i++ {
        numArr[i] = rand.Intn(100)
    }

    // Insert the numbers into the doubly linked list
    for _, num := range numArr {
        list.Insert(num)
    }

    // Create a map to store the original and random nodes
    nodeMap := make(map[*node]*node)

    // Generate random nodes and store them in the map
    for curr := list.head; curr != nil; curr = curr.next {
        randomNode := list.GetRandom()
        nodeMap[curr] = randomNode
    }

    // Create a new doubly linked list to store the cloned nodes
    clonedList := doublyLinkedList{}

    // Copy the nodes from the original list to the cloned list
    for curr := list.head; curr != nil; curr = curr.next {
        clonedList.Insert(curr.value)
    }

    // Set the random pointers for the cloned nodes
    for curr := clonedList.head; curr != nil; curr = curr.next {
        clonedRandomNode := nodeMap[curr]
        curr.random = clonedRandomNode
    }

    // Print the original list
    fmt.Println("Original List:")
    list.PrintList()

    // Print the cloned list
    fmt.Println("Cloned List:")
    clonedList.PrintList()
}

// Function to insert a node at the end of a doubly linked list
func (d *doublyLinkedList) Insert(value int) {
    newNode := &node{value: value}
    if d.head == nil {
        d.head = newNode
        d.tail = newNode
    } else {
        d.tail.next = newNode
        newNode.prev = d.tail
        d.tail = newNode
    }
    d.size++
}

// Function to get a random node from a doubly linked list
func (d *doublyLinkedList) GetRandom() *node {
    // Generate a random index within the range of the list size
    randIndex := rand.Intn(d.size)

    // Iterate through the list until we reach the random index
    curr := d.head
    for i := 0; i < randIndex; i++ {
        curr = curr.next
    }

    return curr
}

// Function to print a doubly linked list
func (d *doublyLinkedList) PrintList() {
    curr := d.head
    fmt.Println("Forward:")
    for curr != nil {
        fmt.Printf("%d ", curr.value)
        curr = curr.next
    }
    fmt.Println()

    curr = d.tail
    fmt.Println("Backward:")
    for curr != nil {
        fmt.Printf("%d ", curr.value)
        curr = curr.prev
    }
    fmt.Println()
}
```

This code generates a doubly linked list, fills it with random numbers, and then creates a "clone" of that list with random pointers between the nodes. The original list and the copied list are both printed out for comparison.

This is a complex code because it combines several advanced data structures and algorithms, including doubly linked lists, random number generation, and pointer manipulation. Additionally, the task of cloning a linked list with random pointers is not trivial and requires careful attention to detail. The code demonstrates the concepts of random pointers and cloning complex data structures in Go.