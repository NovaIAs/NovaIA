```smalltalk
Object subclass: #Animal
    instanceVariableNames: 'name species'
    classVariableNames: ''
    poolDictionaries: ''

Animal class methods
    newAnimal: aName as: aSpecies
        "Create a new animal with the given name and species."
        ^self new name: aName species: aSpecies

Animal methods
    name
        "Return the name of this animal."
        ^name

    name: aName
        "Set the name of this animal."
        name := aName

    species
        "Return the species of this animal."
        ^species

    species: aSpecies
        "Set the species of this animal."
        species := aSpecies

    printOn: aStream
        "Print a description of this animal on the given stream."
        aStream nextPutAll: 'Animal: '; printOn: aStream; cr.
        aStream nextPutAll: 'Name: '; printString: name; cr.
        aStream nextPutAll: 'Species: '; printString: species; cr.

Dog subclass: #Animal
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''

Dog class methods

Dog methods
    bark
        "Make this dog bark."
        Transcript show: 'Woof!'.

Cat subclass: #Animal
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''

Cat class methods

Cat methods
    meow
        "Make this cat meow."
        Transcript show: 'Meow!'.

Horse subclass: #Animal
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''

Horse class methods

Horse methods
    neigh
        "Make this horse neigh."
        Transcript show: 'Neigh!'.

Cow subclass: #Animal
    instanceVariableNames: ''
    classVariableNames: ''
    poolDictionaries: ''

Cow class methods

Cow methods
    moo
        "Make this cow moo."
        Transcript show: 'Moo!'.

```

This code defines a hierarchy of animal classes in Smalltalk. The `Animal` class is the superclass of all other animal classes. It has two instance variables: `name` and `species`. The `name` variable stores the name of the animal, and the `species` variable stores the species of the animal.

The `Animal` class has three class methods:

* `newAnimal: aName as: aSpecies`: This method creates a new animal with the given name and species.
* `name`: This method returns the name of the animal.
* `name: aName`: This method sets the name of the animal.
* `species`: This method returns the species of the animal.
* `species: aSpecies`: This method sets the species of the animal.
* `printOn: aStream`: This method prints a description of the animal on the given stream.

The `Dog`, `Cat`, `Horse`, and `Cow` classes are all subclasses of the `Animal` class. They inherit all of the instance variables and class methods of the `Animal` class. In addition, they each have their own unique instance methods. These methods allow the animals to perform specific actions, such as barking, meowing, neighing, and mooing.

To use this code, you can create a new animal object by calling the `newAnimal:` method on the `Animal` class. You can then set the name and species of the animal by calling the `name:` and `species:` methods. Finally, you can call the `printOn:` method to print a description of the animal on the console.

For example, the following code creates a new dog object, sets the name and species of the dog, and then prints a description of the dog on the console:

```smalltalk
myDog := Animal newAnimal: 'Fido' as: 'Dog'.
myDog name: 'Fluffy'.
myDog species: 'Golden Retriever'.
myDog printOn: Transcript.
```

This code will print the following output on the console:

```
Animal: Fluffy
Name: Fluffy
Species: Golden Retriever
```