```groovy
// Define a class to represent a linked list node
class Node {
    int value
    Node next
}

// Define a class to represent a linked list
class LinkedList {
    Node head
    Node tail

    // Method to add a new node to the end of the list
    void add(int value) {
        // Create a new node
        Node newNode = new Node()
        newNode.value = value

        // If the list is empty, set the head and tail to the new node
        if (head == null) {
            head = newNode
            tail = newNode
        }
        // Otherwise, add the new node to the end of the list
        else {
            tail.next = newNode
            tail = newNode
        }
    }

    // Method to remove a node from the list by value
    void remove(int value) {
        // If the list is empty, return
        if (head == null) {
            return
        }

        // If the head node is the one to be removed, set the head to the next node
        if (head.value == value) {
            head = head.next
        }
        // Otherwise, find the node to be removed and remove it
        else {
            Node currentNode = head
            while (currentNode.next != null) {
                if (currentNode.next.value == value) {
                    currentNode.next = currentNode.next.next
                    break
                }
                currentNode = currentNode.next
            }
        }

        // If the tail node was removed, set the tail to the previous node
        if (tail.value == value) {
            tail = currentNode
        }
    }

    // Method to print the list
    void print() {
        // If the list is empty, return
        if (head == null) {
            return
        }

        // Print the values of the nodes in the list
        Node currentNode = head
        while (currentNode != null) {
            print("${currentNode.value} ")
            currentNode = currentNode.next
        }
        println()
    }
}

// Create a new linked list
LinkedList myList = new LinkedList()

// Add some values to the list
myList.add(1)
myList.add(2)
myList.add(3)
myList.add(4)
myList.add(5)

// Print the list
println("Original list:")
myList.print()

// Remove a value from the list
myList.remove(3)

// Print the list again
println("List after removing the value 3:")
myList.print()

// Add another value to the list
myList.add(6)

// Print the list again
println("List after adding the value 6:")
myList.print()
```

Explanation:

This code defines a linked list data structure and demonstrates some of its basic operations.

1. **Node Class:**
   - The code defines a class called `Node` which represents a single node in the linked list. Each node has two fields: `value` to store the data and `next` to point to the next node in the list.


2. **LinkedList Class:**
   - The `LinkedList` class represents the entire linked list. It has three fields: `head`, `tail`, and `size`.
     - `head` points to the first node in the list.
     - `tail` points to the last node in the list.
     - `size` keeps track of the number of nodes in the list.


3. **Adding a Node:**
   - The `add` method is used to add a new node to the end of the list.
   - It creates a new node, sets its `value` field to the specified value, and adds it to the end of the list.


4. **Removing a Node:**
   - The `remove` method is used to remove a node from the list by its value.
   - It iterates through the list and removes the node with the matching value.
   - If the node is the head or tail of the list, it adjusts the `head` and `tail` pointers accordingly.


5. **Printing the List:**
   - The `print` method is used to print the values of the nodes in the list.
   - It iterates through the list and prints the `value` field of each node.


6. **Usage:**
   - The code then creates a new instance of the `LinkedList` class and adds some values to it.
   - It then demonstrates the `remove` and `add` methods by removing a value and adding another value to the list.
   - Finally, it prints the list again to show the changes.