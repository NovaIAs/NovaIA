```scala
object ComplexScalaCode {

  // Define a sealed trait to represent different types of animals
  sealed trait Animal

  // Define case classes for specific types of animals
  case class Dog(name: String, breed: String) extends Animal
  case class Cat(name: String, furColor: String) extends Animal
  case class Fish(name: String, species: String) extends Animal

  // Define a function to print information about an animal
  def printAnimalInfo(animal: Animal): Unit = {
    animal match {
      case Dog(name, breed) => println(s"$name is a dog of breed $breed")
      case Cat(name, furColor) => println(s"$name is a cat with $furColor fur")
      case Fish(name, species) => println(s"$name is a fish of species $species")
    }
  }

  // Define a function to create a list of animals
  def createAnimalList(): List[Animal] = {
    List(
      Dog("Buddy", "Golden Retriever"),
      Cat("Whiskers", "Tabby"),
      Fish("Nemo", "Clownfish")
    )
  }

  // Define a function to sum the ages of a list of animals
  def sumAnimalAges(animals: List[Animal]): Int = {
    animals.foldLeft(0) { (acc, animal) =>
      acc + animal match {
        case Dog(_, _) => 10 // Assume all dogs are 10 years old
        case Cat(_, _) => 7 // Assume all cats are 7 years old
        case Fish(_, _) => 5 // Assume all fish are 5 years old
      }
    }
  }

  // Define a function to find the oldest animal in a list of animals
  def findOldestAnimal(animals: List[Animal]): Animal = {
    animals.maxBy {
      animal => animal match {
        case Dog(_, _) => 10 // Assume all dogs are 10 years old
        case Cat(_, _) => 7 // Assume all cats are 7 years old
        case Fish(_, _) => 5 // Assume all fish are 5 years old
      }
    }
  }

  // Define a function to group animals by their type
  def groupAnimalsByType(animals: List[Animal]): Map[String, List[Animal]] = {
    animals.groupBy {
      animal => animal match {
        case Dog(_, _) => "Dog"
        case Cat(_, _) => "Cat"
        case Fish(_, _) => "Fish"
      }
    }
  }

  // Define a function to print a summary of animal information
  def printAnimalSummary(animals: List[Animal]): Unit = {
    println("List of Animals:")
    animals.foreach(printAnimalInfo)

    val totalAge = sumAnimalAges(animals)
    println(s"Total age of all animals: $totalAge years")

    val oldestAnimal = findOldestAnimal(animals)
    println(s"Oldest animal: ")
    printAnimalInfo(oldestAnimal)

    val groupedAnimals = groupAnimalsByType(animals)
    println("Animals grouped by type:")
    groupedAnimals.foreach { case (animalType, animals) =>
      println(s"$animalType:")
      animals.foreach(printAnimalInfo)
    }
  }

  // Define the main function
  def main(args: Array[String]): Unit = {
    val animals = createAnimalList()
    printAnimalSummary(animals)
  }
}
```

This code is a complex Scala program that demonstrates various programming concepts and features. It defines a sealed trait `Animal` and three case classes `Dog`, `Cat`, and `Fish` to represent different types of animals. It also defines several functions to work with animals, such as printing animal information, creating a list of animals, summing the ages of animals, finding the oldest animal, and grouping animals by type. The `main` function creates a list of animals and then uses the defined functions to print a summary of animal information.

Here's a breakdown of the code:

1. **Animal Trait and Case Classes:**
   - The `Animal` trait is a sealed trait that represents different types of animals.
   - The `Dog`, `Cat`, and `Fish` case classes extend the `Animal` trait and represent specific types of animals.

2. **Print Animal Information Function:**
   - The `printAnimalInfo` function takes an `Animal` object as input and prints information about the animal based on its type.

3. **Create Animal List Function:**
   - The `createAnimalList` function creates a list of animals containing a dog, a cat, and a fish.

4. **Sum Animal Ages Function:**
   - The `sumAnimalAges` function takes a list of animals as input and sums the ages of the animals based on their types.

5. **Find Oldest Animal Function:**
   - The `findOldestAnimal` function takes a list of animals as input and returns the oldest animal based on their assumed ages.

6. **Group Animals by Type Function:**
   - The `groupAnimalsByType` function takes a list of animals as input and groups the animals by their types.

7. **Print Animal Summary Function:**
   - The `printAnimalSummary` function takes a list of animals as input and prints a summary of animal information, including the list of animals, the total age of all animals, the oldest animal, and the animals grouped by type.

8. **Main Function:**
   - The `main` function creates a list of animals using the `createAnimalList` function and then uses the `printAnimalSummary` function to print a summary of animal information.