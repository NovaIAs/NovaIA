```smalltalk
Class hierarchy:
Animal >> Pet >> Dog >> GoldenRetriever

Animal is the superclass of Pet, which is the superclass of Dog, which is the superclass of GoldenRetriever.
The following code demonstrates how to create instances of these classes and call methods on them:

```smalltalk
anAnimal := Animal new.
aPet := Pet new.
aDog := Dog new.
aGoldenRetriever := GoldenRetriever new.

anAnimal speak.    "Prints "Animal says woof woof!""
aPet speak.        "Prints "Pet says woof woof!""
aDog speak.        "Prints "Dog says woof woof!""
aGoldenRetriever speak.  "Prints "Golden Retriever says woof woof!""
```

Inheritance:
Subclassing allows for code reuse and the ability to extend the functionality of existing classes.
For example, the Dog class inherits from the Pet class, which in turn inherits from the Animal class.
This means that the Dog class has access to all of the methods and instance variables defined in the Pet and Animal classes.
The following code demonstrates how to create a new class called Bulldog that inherits from the Dog class:

```smalltalk
Bulldog := Class new subclass: Dog named: #Bulldog.

Bulldog speak.  "Prints "Bulldog says woof woof!""
```

Polymorphism:
Polymorphism allows objects of different classes to respond to the same message in different ways.
For example, the speak method is defined in the Animal, Pet, Dog, and GoldenRetriever classes, but each class implements the method in a different way.
This allows us to send the speak message to objects of different classes and get different results.
The following code demonstrates how to use polymorphism to send the speak message to objects of different classes:

```smalltalk
animals := OrderedCollection new.
animals add: anAnimal.
animals add: aPet.
animals add: aDog.
animals add: aGoldenRetriever.
animals add: aBulldog.

animals do: [:animal | animal speak].  "Prints "Animal says woof woof!", "Pet says woof woof!", "Dog says woof woof!", "Golden Retriever says woof woof!", and "Bulldog says woof woof!""
```

Message sending:
Message sending is the mechanism by which objects communicate with each other.
To send a message to an object, we use the following syntax:

```smalltalk
[receiver message arguments]
```

The receiver is the object that we are sending the message to.
The message is the name of the method that we want to call.
The arguments are the values that we want to pass to the method.
The following code demonstrates how to send the speak message to anAnimal:

```smalltalk
anAnimal speak.
```

Dynamic binding:
Dynamic binding is the process of determining which method to call when a message is sent to an object.
The method that is called is determined by the class of the object that receives the message.
This allows us to send the same message to objects of different classes and get different results.
The following code demonstrates how to use dynamic binding to send the speak message to objects of different classes:

```smalltalk
animals := OrderedCollection new.
animals add: anAnimal.
animals add: aPet.
animals add: aDog.
animals add: aGoldenRetriever.
animals add: aBulldog.

animals do: [:animal | animal speak].  "Prints "Animal says woof woof!", "Pet says woof woof!", "Dog says woof woof!", "Golden Retriever says woof woof!", and "Bulldog says woof woof!""
```

Late binding:
Late binding is the process of determining the type of an object at runtime.
This allows us to write code that is independent of the specific types of objects that it will be used with.
The following code demonstrates how to use late binding to create an array of animals and then send the speak message to each animal in the array:

```smalltalk
animals := OrderedCollection new.
animals add: anAnimal.
animals add: aPet.
animals add: aDog.
animals add: aGoldenRetriever.
animals add: aBulldog.

animals do: [:animal | animal speak].  "Prints "Animal says woof woof!", "Pet says woof woof!", "Dog says woof woof!", "Golden Retriever says woof woof!", and "Bulldog says woof woof!""
```

Closures:
Closures are blocks of code that can be passed around as values.
This allows us to write code that is more flexible and reusable.
The following code demonstrates how to create a closure and then pass it to the do: method of an array:

```smalltalk
animals := OrderedCollection new.
animals add: anAnimal.
```