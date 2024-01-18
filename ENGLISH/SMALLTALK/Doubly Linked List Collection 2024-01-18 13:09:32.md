```smalltalk

    "Class Category: Collections-Abstract"
    "File: OrderedCollection.st"
    "Generated from OrderedCollection.st by st2cs v0.3"

    OrderedCollection subclass: #DoublyLinkedListedCollection [
        | array |

        "Allocate an empty collection."

        initialize [
            super initialize.
            array := Array new: 0 ]

        "Return the array."

        collectionArray [
            ^array ]

        "Return the size of the collection."

        size [
            ^array size ]

        "Return the object at the given index."

        at: anIndex [
            ^array at: anIndex ]

        "Add the given object to the end of the collection."

        add: anObject [
            self growBy: 1.
            array at: self size put: anObject ]

        "Remove an object from the collection."

        removeObject: anObject [
            self growBy: -1.
            array remove: anObject ]

        "Ensure the array is big enough to accomodate the size."

        growBy: anIncrement [
            array := array prepend: anIncrement grow: anIncrement.
            true ]

        "Return the object at the given index and remove it."

        removeAtIndex: anIndex [
            | anObject |
            anObject := array at: anIndex.
            self growBy: -1.
            array remove: anObject.
            ^anObject ]

        "Return the last object in the collection."

        last [
            ^array last ]

        "Add the given object to the front of the collection."

        prepend: anObject [
            self growBy: 1.
            array add: anObject ]

        "Return the object at the given index."

        at: anIndex put: anObject [
            array at: anIndex put: anObject ]

        "Add the given object to the end of the collection."

        addAll: anArray [
            | n |
            n := anArray size.
            self growBy: n.
            array addAll: anArray startingAt: 1 count: n ]

        "Insert the given object before the given index."

        at: anIndex insert: anObject [
            self growBy: 1.
            array add: anObject before: anIndex ]

        "Add the given object to the end of the collection."

        add: anObject ifAbsent: nilBlock [
            | result |
            result := nilBlock value.
            array add: (result ~~ nil) ? result : anObject ]

        "Return an OrderedCollection with all of the objects in this collection selected by the block."

        select: aBlock [
            | result |
            result := OrderedCollection new.
            self do: [ :each | result add: (aBlock value: each) ifTrue: true ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection rejected by the block."

        reject: aBlock [
            ^self select: [ :each | (aBlock value: each) ifFalse: true ] ]

        "Return an OrderedCollection with all of the objects in this collection collected by the block."

        collect: aBlock [
            | result |
            result := OrderedCollection new.
            self do: [ :each | result addAll: (aBlock value: each) ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection injected into the block."

        inject: aBlock into: value [
            | result |
            result := value.
            self do: [ :each | result := (aBlock value: result value: each) ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection detected by the block."

        detect: aBlock [
            | result |
            result := nil.
            self do: [ :each | (result := (aBlock value: each)) ifTrue: [ ^result ] ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection counted by the block."

        count: aBlock [
            | result |
            result := 0.
            self do: [ :each | result := result + (aBlock value: each) ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection counted by the block."

        sum: aBlock [
            | result |
            result := 0.
            self do: [ :each | result := result + (aBlock value: each) ].
            ^result ]

        "Return an OrderedCollection with all of the objects in this collection sorted by the block."

        sort: aBlock [
            | result |
            result := self collect: [ :each | OrderedCollectionTuple new at: 0 put: each ] asSortedArray: [ :tu1 :tu2 | (aBlock value: tu1 at: 0 value: tu2 at: 0) ] ] asCollection.
            result collect: [ :each | each at: 0 ] ]
    ]