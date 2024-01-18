```scala
// This is a complex Scala code that demonstrates the use of various features of the language, including classes, objects, traits, and higher-order functions.

// Define a trait called `Animal` with an abstract method called `makeSound`.
trait Animal {
  def makeSound(): String
}

// Define a class called `Dog` that extends the `Animal` trait and implements the `makeSound` method.
class Dog extends Animal {
  override def makeSound(): String = "Woof!"
}

// Define a class called `Cat` that extends the `Animal` trait and implements the `makeSound` method.
class Cat extends Animal {
  override def makeSound(): String = "Meow!"
}

// Define a class called `AnimalFactory` that has a factory method called `createAnimal`.
class AnimalFactory {
  def createAnimal(animalType: String): Animal = {
    animalType match {
      case "dog" => new Dog
      case "cat" => new Cat
      case _ => throw new IllegalArgumentException("Invalid animal type: " + animalType)
    }
  }
}

// Define a function called `printAnimalSounds` that takes a list of animals and prints their sounds.
def printAnimalSounds(animals: List[Animal]): Unit = {
  for (animal <- animals) {
    println(animal.makeSound())
  }
}

// Create an instance of the `AnimalFactory` class.
val animalFactory = new AnimalFactory

// Create a list of animals using the factory method.
val animals = List(animalFactory.createAnimal("dog"), animalFactory.createAnimal("cat"))

// Print the sounds of the animals.
printAnimalSounds(animals)
```

Explanation:

1. We start by defining a trait called `Animal` which has an abstract method called `makeSound`. This trait represents the common characteristics of all animals.

2. We then define two classes called `Dog` and `Cat` that extend the `Animal` trait. These classes implement the `makeSound` method with the appropriate sound for each animal.

3. Next, we define a class called `AnimalFactory` which has a factory method called `createAnimal`. This factory method takes in an animal type as a string and returns an instance of the corresponding animal class.

4. We then define a function called `printAnimalSounds` which takes a list of animals and prints their sounds by calling the `makeSound` method on each animal.

5. We create an instance of the `AnimalFactory` class and use it to create a list of animals.

6. Finally, we call the `printAnimalSounds` function to print the sounds of the animals in the list.

This code demonstrates the use of traits, classes, objects, factory methods, and higher-order functions in Scala. It also shows how to use the `match` expression to handle different cases in a concise manner.