```swift
import Foundation

class Node<T> {
    var value: T
    var next: Node<T>?

    init(value: T, next: Node<T>? = nil) {
        self.value = value
        self.next = next
    }
}

class LinkedList<T> {
    var head: Node<T>?
    var tail: Node<T>?

    var isEmpty: Bool {
        return head == nil
    }

    func append(_ value: T) {
        let newNode = Node(value: value)

        if let tailNode = tail {
            tailNode.next = newNode
        } else {
            head = newNode
        }

        tail = newNode
    }

    func removeFirst() -> T? {
        guard let headNode = head else {
            return nil
        }

        head = headNode.next

        if head == nil {
            tail = nil
        }

        return headNode.value
    }

    func removeLast() -> T? {
        guard let tailNode = tail else {
            return nil
        }

        var previousNode: Node<T>?

        var currentNode = head

        while currentNode?.next != nil {
            previousNode = currentNode
            currentNode = currentNode?.next
        }

        previousNode?.next = nil
        tail = previousNode

        if tail == nil {
            head = nil
        }

        return tailNode.value
    }

    func insert(after node: Node<T>, value: T) {
        guard let nextNode = node.next else {
            append(value)
            return
        }

        let newNode = Node(value: value, next: nextNode)
        node.next = newNode

        if tail == node {
            tail = newNode
        }
    }

    func remove(node: Node<T>) -> T? {
        guard node !== head else {
            return removeFirst()
        }

        guard node !== tail else {
            return removeLast()
        }

        var previousNode: Node<T>?

        var currentNode = head

        while currentNode !== node {
            previousNode = currentNode
            currentNode = currentNode?.next
        }

        previousNode?.next = node.next

        if previousNode?.next == nil {
            tail = previousNode
        }

        return node.value
    }

    func search(value: T) -> Node<T>? {
        var currentNode = head

        while currentNode != nil {
            if currentNode?.value == value {
                return currentNode
            }

            currentNode = currentNode?.next
        }

        return nil
    }
}

struct Contact {
    var name: String
    var email: String
}

let contacts = LinkedList<Contact>()

contacts.append(Contact(name: "John", email: "john@example.com"))
contacts.append(Contact(name: "Mary", email: "mary@example.com"))
contacts.append(Contact(name: "Bob", email: "bob@example.com"))

print(contacts.head?.value) // Prints "John"
print(contacts.tail?.value) // Prints "Bob"

let removedFirstContact = contacts.removeFirst() // Removes "John"
print(removedFirstContact?.name) // Prints "John"

let removedLastContact = contacts.removeLast() // Removes "Bob"
print(removedLastContact?.name) // Prints "Bob"

contacts.insert(after: contacts.head!, value: Contact(name: "Alice", email: "alice@example.com"))

let removedMiddleContact = contacts.remove(node: contacts.head!.next!) // Removes "Mary"
print(removedMiddleContact?.name) // Prints "Mary"

let searchedContact = contacts.search(value: Contact(name: "Alice", email: "alice@example.com"))
print(searchedContact?.value) // Prints "Alice"

```

This is a Swift implementation of a linked list data structure. A linked list is a linear data structure that consists of a sequence of nodes, where each node contains a value and a reference to the next node in the sequence. The linked list allows for efficient insertion and deletion of elements at any position in the list.

The LinkedList class has a head and a tail attribute, which point to the first and last nodes in the list, respectively. It also has a method called append, which adds a new value to the end of the list. The removeFirst and removeLast methods remove the first and last nodes from the list, respectively. The insert method inserts a new value after a given node in the list. The remove method removes a given node from the list. The search method searches for a value in the list and returns the node that contains the value.

The Contact struct is a simple data structure that represents a contact with a name and an email address.

The code first creates a linked list of contacts and adds three contacts to the list. It then prints the first and last contacts in the list. The code then removes the first and last contacts from the list and prints the removed contacts. It then inserts a new contact after the head of the list. The code then removes the middle contact from the list and prints the removed contact. Finally, the code searches for a contact in the list and prints the found contact.