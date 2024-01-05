```smalltalk
Object subclass: ClassHierarchy [
    | classHierarchy |
    classHierarchy := OrderedCollection new.

    class addSubclass: newClass [
        | subclass |
        subclass := newClass.
        classHierarchy at: classHierarchy size + 1 put: subclass.
        subclass superclass: class.
        classHierarchy at: classHierarchy size put: class.
    ]

    subclass allSuperclasses [
        | superclass |
        superclass := self superclass.
        superclass ifNotNil: [
            ^ superclass allSuperclasses, superclass.
        ].  "base case"
        ^ OrderedCollection new.
    ]

    subclass ancestry [
        ^ self allSuperclasses, self.
    ]

    subclass hierarchy [
        classHierarchy size > 0 ifTrue: [
            classHierarchy at: classHierarchy size := self.
        ].  "base case"
        self superclass ifNotNil: [
            self superclass hierarchy.
            classHierarchy at: classHierarchy size := self.
        ].
        ^ classHierarchy.
    ]

    subclass depth [
        ^ self hierarchy size - 1.
    ]

    subclass ancestors [
        ^ (self hierarchy copyWithout: self) reverse.
    ]

    subclass descendants [
        | descendants |
        descendants := OrderedCollection new.
        self subclasses do: [ :subclass |
            descendants addAll: subclass descendants.
            descendants add: subclass.
        ].
        ^ descendants.
    ]

    subclass siblings [
        ^ self superclass ifNotNil: [ self superclass subclasses ] ifNil: [ OrderedCollection new ].
    ]

    subclass cousins [
        ^ self siblings flatMap: [ :sibling | sibling siblings ] asOrderedCollection.
    ]

    subclass leaf [
        ^ self subclasses isEmpty.
    ]

    subclass root [
        ^ self superclass isNil.
    ]

    subclass height [
        | height |
        height := 0.
        self subclasses do: [ :subclass |
            height := height max: subclass height + 1.
        ].
        ^ height.
    ]

    subclass width [
        | width |
        width := 0.
        self siblings do: [ :sibling |
            width := width max: sibling descendants size.
        ].
        ^ width.
    ]

    subclass size [
        ^ self descendants size.
    ]

    subclass printHierarchy [
        | stream |
        stream := Transcript current.
        stream cr; nextPutAll: 'Class Hierarchy for '; print: self name.
        stream cr; nextPutAll: '  Depth: '; print: self depth.
        stream cr; nextPutAll: '  Height: '; print: self height.
        stream cr; nextPutAll: '  Width: '; print: self width.
        stream cr; nextPutAll: '  Size: '; print: self size.
        stream cr; nextPutAll: '  Ancestors: '; print: self ancestors asString.
        stream cr; nextPutAll: '  Descendants: '; print: self descendants asString.
        stream cr; nextPutAll: '  Siblings: '; print: self siblings asString.
        stream cr; nextPutAll: '  Cousins: '; print: self cousins asString.
        stream cr; nextPutAll: '  Hierarchy: '; print: self hierarchy asString.
    ]
]

ClassHierarchy subclass: ClassHierarchyTester [

    self run.

    run [
        | aClass |
        aClass := Class new named: 'A'.
        aClass addSubclass: Class new named: 'B'.
        aClass addSubclass: Class new named: 'C'.
        B addSubclass: Class new named: 'D'.
        B addSubclass: Class new named: 'E'.
        C addSubclass: Class new named: 'F'.
        G := Class new named: 'G'.

        A printHierarchy.
        B printHierarchy.
        D printHierarchy.
        F printHierarchy.
        G printHierarchy.

        (A width = B width) ifFalse: [ self error: 'A width not equal to B width' ].
        (A height = C height) ifFalse: [ self error: 'A height not equal to C height' ].
        (A descendants size = 6) ifFalse: [ self error: 'A descendants size not equal to 6' ].
        (B ancestors size = 1) ifFalse: [ self error: 'B ancestors size not equal to 1' ].
        (D siblings size = 1) ifFalse: [ self error: 'D siblings size not equal to 1' ].
        (F cousins size = 2) ifFalse: [ self error: 'F cousins size not equal to 2' ].
        (G leaf) ifTrue: [ self error: 'G is a leaf' ].
        (A root) ifFalse: [ self error: 'A is not a root' ].
    ]
]
```

This code defines a class hierarchy in Smalltalk, which is a class-based programming language. The ClassHierarchy class defines a way to organize and manipulate classes in a hierarchical structure. It includes methods for adding subclasses, finding ancestors and descendants, printing the hierarchy, and calculating various metrics such as depth, height, width, and size.

The ClassHierarchyTester class is a test class that creates a sample class hierarchy and then uses the methods of the ClassHierarchy class to print the hierarchy and calculate various metrics. It also includes a method run that runs the tests and checks for errors.

Here's a breakdown of the code:

1. The ClassHierarchy class is defined as a subclass of Object, which is the root class in Smalltalk.

2. The classHierarchy instance variable is initialized to an OrderedCollection, which is a dynamic array in Smalltalk.

3. The addSubclass: method adds a new subclass to the class hierarchy and updates the classHierarchy instance variable accordingly.

4. The allSuperclasses method returns a collection of all superclasses of the receiver class, starting from the immediate superclass and going up the hierarchy.

5. The ancestry method returns a collection of all ancestors of the receiver class, including the receiver class itself.

6. The hierarchy method returns a collection of all classes in the hierarchy, starting from the receiver class and going down the hierarchy.

7. The depth method returns the depth of the receiver class in the hierarchy, which is the number of levels from the receiver class to the root class.

8. The ancestors method returns a collection of all ancestors of the receiver class, including the receiver class itself.

9. The descendants method returns a collection of all descendants of the receiver class, including the receiver class itself.

10. The siblings method returns a collection of all siblings of the receiver class, which are the other subclasses of the receiver class's superclass.

11. The cousins method returns a collection of all cousins of the receiver class, which are the siblings of the receiver class's siblings.

12. The leaf method returns true if the receiver class is a leaf in the hierarchy, meaning it has no subclasses.

13. The root method returns true if the receiver class is the root class in the hierarchy, meaning it has no superclass.

14. The height method returns the height of the receiver class in the hierarchy, which is the maximum depth of any of its descendants.

15. The width method returns the width of the receiver class in the hierarchy, which is the maximum number of descendants at any level below the receiver class.

16. The size method returns the size of the receiver class in the hierarchy, which is the total number of classes in the hierarchy, including the receiver class.

17. The printHierarchy method prints information about the receiver class and its hierarchy, including depth, height, width, size, ancestors, descendants, siblings, cousins, and the hierarchy itself.

18. The ClassHierarchyTester class is defined as a subclass of ClassHierarchy.

19. The run method in the ClassHierarchyTester class creates a sample class hierarchy and then uses the methods of the ClassHierarchy class to print the hierarchy and calculate various metrics.

20. The run method also includes error checking to ensure that the results of the tests are correct.

To use this code, you can create an instance of the ClassHierarchyTester class and call the run method. This will create a sample class hierarchy, print the hierarchy, and calculate various metrics. You can also use the methods of the ClassHierarchy class to manipulate and query the class hierarchy.