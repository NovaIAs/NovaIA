```cool
-- Define a class called "LinkedList"
class LinkedList {
    var head: Node; -- The head of the linked list
    var tail: Node; -- The tail of the linked list

    -- Constructor for the linked list
    new() {
        head := nil;
        tail := nil;
    }

    -- Add a new element to the end of the linked list
    insert(elem: Int) {
        var new_node: Node := new Node(elem);
        if head = nil then {
            head := new_node;
            tail := new_node;
        } else {
            tail.next := new_node;
            tail := new_node;
        }
    }

    -- Remove the first occurrence of an element from the linked list
    remove(elem: Int) {
        if head = nil then {
            return;
        }

        if head.elem = elem then {
            head := head.next;
            if head = nil then {
                tail := nil;
            }
            return;
        }

        var prev: Node := head;
        var curr: Node := head.next;

        while curr != nil and curr.elem != elem do {
            prev := curr;
            curr := curr.next;
        }

        if curr != nil then {
            prev.next := curr.next;
            if curr = tail then {
                tail := prev;
            }
        }
    }

    -- Find the first occurrence of an element in the linked list
    find(elem: Int): Node {
        var curr: Node := head;

        while curr != nil and curr.elem != elem do {
            curr := curr.next;
        }

        return curr;
    }

    -- Print the linked list
    print() {
        var curr: Node := head;

        while curr != nil do {
            print(curr.elem);
            print(" ");
            curr := curr.next;
        }

        println();
    }
}

-- Define a class called "Node"
class Node {
    var elem: Int; -- The element stored in the node
    var next: Node; -- The next node in the linked list

    -- Constructor for the node
    new(elem: Int) {
        this.elem := elem;
        next := nil;
    }
}

-- Create a new linked list
var list: LinkedList := new LinkedList();

-- Insert some elements into the linked list
for i in 1 to 10 do {
    list.insert(i);
}

-- Print the linked list
list.print();

-- Remove the first occurrence of the element 5 from the linked list
list.remove(5);

-- Print the linked list
list.print();

-- Find the first occurrence of the element 7 in the linked list
var found_node: Node := list.find(7);

if found_node = nil then {
    println("Element 7 not found in the linked list.");
} else {
    println("Element 7 found at index " + found_node.elem);
}
```

Explanation:

This code defines a class called "LinkedList" that represents a linked list. A linked list is a data structure that stores data in a linear fashion, where each element is linked to the next element in the list.

The "LinkedList" class has three instance variables: "head", "tail", and "next". The "head" variable points to the first element in the list, and the "tail" variable points to the last element in the list. The "next" variable is used to link the elements of the list together.

The "LinkedList" class has several methods, including "insert", "remove", "find", "print", and "new". The "insert" method adds a new element to the end of the list, the "remove" method removes the first occurrence of an element from the list, the "find" method finds the first occurrence of an element in the list, the "print" method prints the elements of the list, and the "new" method creates a new linked list object.

The code also defines a class called "Node" that represents a node in a linked list. A node is an object that stores a single element of data and a reference to the next node in the list.

The "Node" class has two instance variables: "elem" and "next". The "elem" variable stores the data element, and the "next" variable points to the next node in the list.

The "Node" class has a constructor that takes a single argument, which is the data element to be stored in the node. The constructor also sets the "next" variable to nil, indicating that this is the last node in the list.

The main program creates a new linked list object and inserts the numbers from 1 to 10 into the list. The program then prints the list, removes the first occurrence of the element 5 from the list, prints the list again, and finally finds the first occurrence of the element 7 in the list and prints its position.

This code is a complex example of how to implement a linked list in COOL. It demonstrates how to create a new linked list, insert elements into the list, remove elements from the list, find elements in the list, and print the elements of the list.