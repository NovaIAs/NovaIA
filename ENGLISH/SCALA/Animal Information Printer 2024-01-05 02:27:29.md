```scala
// Define a sealed trait for describing types of animals
sealed trait Animal

// Define concrete subclasses of Animal
case class Dog(name: String, breed: String) extends Animal
case class Cat(name: String, furColor: String) extends Animal
case class Bird(name: String, species: String) extends Animal

// Define a function to print information about an animal
def printAnimalInfo(animal: Animal): Unit = {
  animal match {
    case Dog(name, breed) =>
      println(s"This dog's name is $name and its breed is $breed.")
    case Cat(name, furColor) =>
      println(s"This cat's name is $name and its fur color is $furColor.")
    case Bird(name, species) =>
      println(s"This bird's name is $name and its species is $species.")
  }
}

// Define a list of animals
val animals: List[Animal] = List(
  Dog("Buddy", "Golden Retriever"),
  Cat("Whiskers", "Tabby"),
  Bird("Tweety", "Canary")
)

// Iterate over the list of animals and print information about each one
animals.foreach(printAnimalInfo)
```

Explanation:

1. **Sealed Trait and Concrete Subclasses**:
   - We define a sealed trait `Animal` to represent different types of animals.
   - We create concrete subclasses of `Animal`, such as `Dog`, `Cat`, and `Bird`, to represent specific types of animals.

2. **Function to Print Animal Information**:
   - We define a function `printAnimalInfo` that takes an `Animal` object as input and prints information about the animal.

3. **List of Animals**:
   - We create a list of `Animal` objects, each representing a specific animal with its attributes, such as name, breed, fur color, and species.

4. **Iterating Over the List**:
   - We use the `foreach` method on the list of animals to iterate over each animal and pass it to the `printAnimalInfo` function.

5. **Printing Animal Information**:
   - Inside the `printAnimalInfo` function, we use the `match` expression to check the type of the animal and print specific information based on the type.

When you run this code, it will print information about each animal in the list, including its name and specific attributes, such as breed for `Dog`, fur color for `Cat`, and species for `Bird`. This demonstrates the use of a sealed trait, concrete subclasses, pattern matching, and iteration in Scala.