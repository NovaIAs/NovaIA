```
object ComplexScalaCode {

  // Define a sealed trait to represent different types of animals
  sealed trait Animal

  // Define case classes for specific types of animals
  case class Dog(name: String, breed: String) extends Animal
  case class Cat(name: String, color: String) extends Animal
  case class Bird(name: String, species: String) extends Animal

  // Define a function to print information about an animal
  def printAnimalInfo(animal: Animal): Unit = {
    animal match {
      case Dog(name, breed) => println(s"Dog: $name, Breed: $breed")
      case Cat(name, color) => println(s"Cat: $name, Color: $color")
      case Bird(name, species) => println(s"Bird: $name, Species: $species")
    }
  }

  // Define a function to create a list of animals
  def createAnimalList(): List[Animal] = {
    List(
      Dog("Buddy", "Golden Retriever"),
      Cat("Whiskers", "Siamese"),
      Bird("Tweety", "Canary"),
      Dog("Max", "German Shepherd"),
      Cat("Mittens", "Calico"),
      Bird("Polly", "Parrot")
    )
  }

  // Define a function to filter animals by type
  def filterAnimalsByType(animals: List[Animal], animalType: String): List[Animal] = {
    animals.filter {
      case Dog(_, _) => animalType == "dog"
      case Cat(_, _) => animalType == "cat"
      case Bird(_, _) => animalType == "bird"
    }
  }

  // Define a function to calculate the average age of animals
  def calculateAverageAge(animals: List[Animal]): Double = {
    val ages = animals.map {
      case Dog(_, _) => 10
      case Cat(_, _) => 15
      case Bird(_, _) => 5
    }
    ages.sum / ages.length
  }

  // Define a function to find the oldest animal
  def findOldestAnimal(animals: List[Animal]): Animal = {
    animals.maxBy {
      case Dog(_, _) => 10
      case Cat(_, _) => 15
      case Bird(_, _) => 5
    }
  }

  // Define a function to group animals by type
  def groupAnimalsByType(animals: List[Animal]): Map[String, List[Animal]] = {
    animals.groupBy {
      case Dog(_, _) => "dog"
      case Cat(_, _) => "cat"
      case Bird(_, _) => "bird"
    }
  }

  // Define a function to print animal information in a table format
  def printAnimalTable(animals: List[Animal]): Unit = {
    println("-------------------------------------------------")
    println("| Name | Type | Breed/Color/Species | Age |")
    println("-------------------------------------------------")
    animals.foreach {
      case Dog(name, breed) => println(f"| $name%10s | Dog%7s | $breed%15s | 10%3d |")
      case Cat(name, color) => println(f"| $name%10s | Cat%7s | $color%17s | 15%3d |")
      case Bird(name, species) => println(f"| $name%10s | Bird%6s | $species%15s | 5%3d |")
    }
    println("-------------------------------------------------")
  }

  // Main function to execute the program
  def main(args: Array[String]): Unit = {

    // Create a list of animals
    val animals = createAnimalList()

    // Print information about each animal
    println("Printing information about each animal:")
    animals.foreach(printAnimalInfo)

    // Filter animals by type
    val dogs = filterAnimalsByType(animals, "dog")
    val cats = filterAnimalsByType(animals, "cat")
    val birds = filterAnimalsByType(animals, "bird")

    // Print filtered animal lists
    println("\nFiltered lists of animals:")
    println("Dogs:")
    dogs.foreach(printAnimalInfo)
    println("Cats:")
    cats.foreach(printAnimalInfo)
    println("Birds:")
    birds.foreach(printAnimalInfo)

    // Calculate the average age of animals
    val averageAge = calculateAverageAge(animals)
    println(f"\nAverage age of animals: $averageAge%.2f years")

    // Find the oldest animal
    val oldestAnimal = findOldestAnimal(animals)
    println("\nOldest animal:")
    printAnimalInfo(oldestAnimal)

    // Group animals by type
    val groupedAnimals = groupAnimalsByType(animals)
    println("\nAnimals grouped by type:")
    groupedAnimals.foreach { case (animalType, animals) =>
      println(s"$animalType:")
      animals.foreach(printAnimalInfo)
    }

    // Print animal information in a table format
    println("\nPrinting animal information in a table format:")
    printAnimalTable(animals)
  }
}
```

**Explanation:**

This Scala code is a complex and comprehensive program that demonstrates various features of the Scala programming language. It involves creating a list of animals, filtering animals by type, calculating the average age of animals, finding the oldest animal, grouping animals by type, and printing animal information in a table format.

Here's a detailed explanation of the code:

1. **Animal Trait and Case Classes:**
   - We define a sealed trait called `Animal` to represent different types of animals.
   - We create case classes `Dog`, `Cat`, and `Bird` that extend the `Animal` trait. Each case class represents a specific type of animal with its own properties.

2. **Printing Animal Information:**
   - We define a function `printAnimalInfo` that takes an `Animal` object as input and prints its information.

3. **Creating Animal List:**
   - We define a function `createAnimalList` that creates a list of `Animal` objects. This list contains instances of `Dog`, `Cat`, and `Bird` classes.

4. **Filtering Animals by Type:**
   - We define a function `filterAnimalsByType` that takes a list of `Animal` objects and a string representing the animal type as input. It filters the list and returns a new list containing only animals of the specified type.

5. **Calculating Average Age:**
   - We define a function `calculateAverageAge` that takes a list of `Animal` objects as input. It calculates and returns the average age of the animals in the list.

6. **Finding Oldest Animal:**
   - We define a function `findOldestAnimal` that takes a list of `Animal` objects as input. It finds and returns the oldest animal in the list.

7. **Grouping Animals by Type:**
   - We define a function `groupAnimalsByType` that takes a list of `Animal` objects as input. It groups the animals by their type and returns a `Map` where keys are animal types and values are lists of animals of that type.

8. **Printing Animal Table:**
   - We define a function `printAnimalTable` that takes a list of `Animal` objects as input. It prints the animal information in a tabular format, displaying the animal's name, type, breed/color/species, and age.

9. **Main Function:**
   - The `main` function is the entry point of the program.
   - It calls the `createAnimalList` function to create a list of animals.
   - It then calls various other functions to demonstrate different features of the program, such as printing animal information, filtering animals by type, calculating average age, finding the oldest animal, grouping animals by type, and printing animal information in a table format.

This code showcases various Scala features, including traits, case classes, pattern matching, higher-order functions, and functional programming techniques. It provides a solid example of how to manipulate and process data in Scala and demonstrates the language's expressiveness and versatility.