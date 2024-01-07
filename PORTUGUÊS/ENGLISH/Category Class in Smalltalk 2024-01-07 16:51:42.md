```smalltalk
"A very large and differentiated code, which will hardly be repeated again, in Smalltalk."

Class Category >>
    superclass: Object.

    classVariableNames: 'name'.

    instanceVariableNames: 'products'.

    initialize: aName
        "Initialize a new category with the given name and no products."

        super initialize.
        name := aName.
        products := OrderedCollection new.
```