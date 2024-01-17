```smalltalk

// Define the class Node
Node subclass: MyNode [
  // Define the instance variables
  ^x     // The value of the node
  ^next  // The next node in the list
]

// Define the constructor
MyNode new [
  // Set the instance variables to the given values
  ^super new
      x: anObject
      next: aNode
]

// Define the method to add a new node to the end of the list
MyNode addNode: anObject [
  // Check if the next node is nil
  (self next) isNil ifTrue: [
    // If it is, set the next node to the new node
    self next: anObject
  ]
  ifFalse: [
    // If it is not, recurse on the next node
    (self next) addNode: anObject
  ]
]

// Define the method to remove a node from the list
MyNode removeNode: anObject [
  // Check if the next node is nil
  (self next) isNil ifTrue: [
    // If it is, return nil
    ^nil
  ]

  // Check if the value of the next node is equal to the given object
  (self next) x = anObject ifTrue: [
    // If it is, set the next node to the next node of the next node
    self next: (self next) next
  ]
  ifFalse: [
    // If it is not, recurse on the next node
    (self next) removeNode: anObject
  ]
]

// Define the method to find a node in the list
MyNode findNode: anObject [
  // Check if the value of the node is equal to the given object
  self x = anObject ifTrue: [
    // If it is, return the node
    ^self
  ]

  // Check if the next node is nil
  (self next) isNil ifTrue: [
    // If it is, return nil
    ^nil
  ]

  // If it is not, recurse on the next node
  (self next) findNode: anObject
]

// Define the method to print the list
MyNode printList [
  // Print the value of the node
  Transcript show: self x.

  // Check if the next node is nil
  (self next) isNil ifTrue: [
    // If it is, print a newline
    Transcript cr.
  ]
  ifFalse: [
    // If it is not, recurse on the next node
    (self next) printList
  ]
]

// Create a new list
aList := MyNode new.

// Add some nodes to the list
aList addNode: 1.
aList addNode: 2.
aList addNode: 3.
aList addNode: 4.
aList addNode: 5.

// Print the list
aList printList.

// Remove a node from the list
aList removeNode: 3.

// Print the list again
aList printList.

// Find a node in the list
aNode := aList findNode: 4.

// Print the value of the node
Transcript show: aNode x.

```

This code defines a class called MyNode, which is a subclass of the built-in class Node. MyNode has two instance variables: x and next. x stores the value of the node, and next stores the next node in the list.

The constructor for MyNode takes two arguments: anObject and aNode. anObject is the value of the node, and aNode is the next node in the list.

The addNode: method adds a new node to the end of the list. It checks if the next node is nil. If it is, it sets the next node to the new node. If it is not, it recurses on the next node.

The removeNode: method removes a node from the list. It checks if the next node is nil. If it is, it returns nil. It then checks if the value of the next node is equal to the given object. If it is, it sets the next node to the next node of the next node. If it is not, it recurses on the next node.

The findNode: method finds a node in the list. It checks if the value of the node is equal to the given object. If it is, it returns the node. It then checks if the next node is nil. If it is, it returns nil. If it is not, it recurses on the next node.

The printList method prints the list. It prints the value of the node. It then checks if the next node is nil. If it is, it prints a newline. If it is not, it recurses on the next node.

The code then creates a new list called aList. It then adds some nodes to the list using the addNode: method. It then prints the list using the printList method.

The code then removes a node from the list using the removeNode: method. It then prints the list again using the printList method.

The code then finds a node in the list using the findNode: method. It then prints the value of the node.