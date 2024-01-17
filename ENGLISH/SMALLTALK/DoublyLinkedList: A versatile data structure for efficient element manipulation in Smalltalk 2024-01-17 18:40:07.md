```smalltalk

// Class definition for DoublyLinkedList
class DoublyLinkedList {
    slots:
        first, // The first node in the list
        last,  // The last node in the list
        size; // The number of elements in the list

    methods:
        [
            // Constructor
            new
            [ | self |
                self first: nil.
                self last: nil.
                self size: 0.
                self
            ],

            // Adds an element to the end of the list
            append: anElement
            [ | self |
                | new_node |
                new_node := self basicNew.
                new_node data: anElement.
                self last isEmpty
                    ifTrue: [ self first: new_node. self last: new_node ]
                    ifFalse: [ self last next: new_node. new_node prev: self last ].
                self size: self size + 1
            ],

            // Inserts an element at a given index in the list
            insert: anElement at: anIndex
            [ | self |
                | curr_node |
                self checkIndex: anIndex.
                curr_node := self nodeAtIndex: anIndex.
                curr_node isEmpty
                    ifTrue: [ self append: anElement ]
                    ifFalse: [
                        | new_node |
                        new_node := self basicNew.
                        new_node data: anElement.
                        new_node prev: curr_node prev.
                        curr_node prev isNil
                            ifTrue: [ self first: new_node ]
                            ifFalse: [ curr_node prev next: new_node ].
                        new_node next: curr_node.
                        curr_node prev: new_node.
                        self size: self size + 1
                    ]
            ],

            // Removes the element at a given index from the list
            removeAtIndex: anIndex
            [ | self |
                | curr_node |
                self checkIndex: anIndex.
                curr_node := self nodeAtIndex: anIndex.
                curr_node isEmpty
                    ifTrue: [ self error: 'Index out of bounds' ]
                    ifFalse: [
                        curr_node next isEmpty
                            ifTrue: [ self last: curr_node prev ]
                            ifFalse: [ curr_node next prev: curr_node prev ].
                        curr_node prev isEmpty
                            ifTrue: [ self first: curr_node next ]
                            ifFalse: [ curr_node prev next: curr_node next ].
                        self size: self size - 1.
                        curr_node free
                    ]
            ],

            // Gets the element at a given index in the list
            at: anIndex
            [ | self |
                | curr_node |
                self checkIndex: anIndex.
                curr_node := self nodeAtIndex: anIndex.
                curr_node isEmpty
                    ifTrue: [ self error: 'Index out of bounds' ]
                    ifFalse: [ curr_node data ]
            ],

            // Sets the element at a given index in the list
            put: anElement at: anIndex
            [ | self |
                | curr_node |
                self checkIndex: anIndex.
                curr_node := self nodeAtIndex: anIndex.
                curr_node isEmpty
                    ifTrue: [ self error: 'Index out of bounds' ]
                    ifFalse: [ curr_node data: anElement ]
            ],

            // Gets the size of the list
            size
            [ | self |
                self size
            ],

            // Checks if the list is empty
            isEmpty
            [ | self |
                self size = 0
            ],

            // Clears the list
            clear
            [ | self |
                | curr_node, next_node |
                curr_node := self first.
                [ curr_node isEmpty ] whileFalse: [
                    next_node := curr_node next.
                    curr_node free.
                    curr_node := next_node
                ].
                self first: nil.
                self last: nil.
                self size: 0
            ],

            // Prints the list's elements
            print
            [ | self |
                | curr_node |
                Transcript show: '[ '.
                curr_node := self first.
                [ curr_node isEmpty ] whileFalse: [
                    Transcript show: curr_node data; show: ' '.
                    curr_node := curr_node next
                ].
                Transcript show: ']'
            ],

            // Converts the list to an array
            toArray
            [ | self |
                | result |
                result := Array new: self size.
                self forEach: [ :each | result at: (self indexOf: each) + 1 put: each ].
                result
            ],

            // Creates a new node with the given data
            basicNew
            [ | self |
                Object basicNew: #DoublyLinkedListNode
            ],

            // Gets the node at a given index in the list
            nodeAtIndex: anIndex
            [ | self |
                | curr_node, i |
                curr_node := self first.
                i := 1.
                [ curr_node isEmpty ] whileFalse: [
                    i = i + 1.
                    i = anIndex
                        ifTrue: [ ^curr_node ]
                        ifFalse: [ curr_node := curr_node next ]
                ].
                nil
            ],

            // Checks if an index is valid for the list
            checkIndex: anIndex
            [ | self |
                anIndex < 1 or: [ anIndex > self size ]
                    ifTrue: [ self error: 'Index out of bounds' ]
            ],

            // Raises an error with the given message
            error: aMessage
            [ | self |
                Transcript show: aMessage newLine.
                self halt
            ]
        ]
}

// Class definition for DoublyLinkedListNode
class DoublyLinkedListNode {
    slots:
        data, // The data stored in the node
        next, // The next node in the list
        prev; // The previous node in the list

    methods:
        [
            // Constructor
            new: aData
            [ | self |
                self data: aData.
                self next: nil.
                self prev: nil.
                self
            ],

            // Gets the data stored in the node
            data
            [ | self |
                self data
            ],

            // Sets the data stored in the node
            data: aData
            [ | self |
                self data: aData
            ],

            // Gets the next node in the list
            next
            [ | self |
                self next
            ],

            // Sets the next node in the list
            next: aNode
            [ | self |
                self next: aNode
            ],

            // Gets the previous node in the list
            prev
            [ | self |
                self prev
            ],

            // Sets the previous node in the list
            prev: aNode
            [ | self |
                self prev: aNode
            ]
        ]
}

```