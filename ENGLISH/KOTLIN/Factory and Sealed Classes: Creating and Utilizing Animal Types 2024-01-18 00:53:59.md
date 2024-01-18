```kotlin
// Define a sealed class to represent different types of animals
sealed class Animal {
    // Companion object to define factory methods
    companion object {
        fun create(type: String): Animal {
            return when (type) {
                "Dog" -> Dog()
                "Cat" -> Cat()
                "Bird" -> Bird()
                else -> throw IllegalArgumentException("Invalid animal type: $type")
            }
        }
    }

    // Abstract method to get the sound the animal makes
    abstract fun makeSound(): String
}

// Define a derived class representing a dog
class Dog : Animal() {
    override fun makeSound(): String {
        return "Woof!"
    }
}

// Define a derived class representing a cat
class Cat : Animal() {
    override fun makeSound(): String {
        return "Meow!"
    }
}

// Define a derived class representing a bird
class Bird : Animal() {
    override fun makeSound(): String {
        return "Chirp!"
    }
}

// Define a function to print the sound an animal makes
fun printAnimalSound(animal: Animal) {
    println(animal.makeSound())
}

// Create a list of animal types
val animalTypes = listOf("Dog", "Cat", "Bird", "Snake", "Elephant")

// Iterate over the list of animal types and create a new animal instance for each type
val animals = animalTypes.map { Animal.create(it) }

// Print the sound each animal makes
animals.forEach { printAnimalSound(it) }
```

This code defines a sealed class called `Animal` with abstract methods `makeSound()`. This class has companion object, which defines factory methods to create different types of animals, such as `Dog`, `Cat`, and `Bird`.

The code also defines a function called `printAnimalSound()` to print the sound an animal makes.

Then, the code creates a list of animal types and uses the `map()` function to create a list of animal instances. Finally, the code uses the `forEach()` function to iterate over the list of animals and print the sound each animal makes.