```kotlin
// Define a sealed class to represent different types of animals
sealed class Animal {
    abstract val name: String
    abstract val sound: String

    // Companion object to create instances of the sealed class
    companion object {
        fun createAnimal(type: String, name: String): Animal {
            return when (type) {
                "Cat" -> Cat(name)
                "Dog" -> Dog(name)
                "Lion" -> Lion(name)
                else -> throw IllegalArgumentException("Invalid animal type: $type")
            }
        }
    }
}

// Define a data class to represent cats
data class Cat(override val name: String) : Animal() {
    override val sound: String = "Meow"
}

// Define a data class to represent dogs
data class Dog(override val name: String) : Animal() {
    override val sound: String = "Woof"
}

// Define a data class to represent lions
data class Lion(override val name: String) : Animal() {
    override val sound: String = "Roar"
}

// Define a function to print the animal's name and sound
fun printAnimalInfo(animal: Animal) {
    println("Name: ${animal.name}")
    println("Sound: ${animal.sound}")
}

// Main function to create and print information about different animals
fun main() {
    val cat = Animal.Companion.createAnimal("Cat", "Kitty")
    val dog = Animal.Companion.createAnimal("Dog", "Fido")
    val lion = Animal.Companion.createAnimal("Lion", "Simba")

    printAnimalInfo(cat)
    printAnimalInfo(dog)
    printAnimalInfo(lion)
}
```

Explanation:

1. We define a sealed class `Animal` to represent different types of animals. It has an abstract property `name` and an abstract property `sound`.

2. We define three data classes `Cat`, `Dog`, and `Lion` that extend the `Animal` class. Each data class has a constructor that takes the animal's name as an argument.

3. We define a companion object for the `Animal` class that contains a factory method `createAnimal`. This method takes the animal type and name as arguments and returns an instance of the appropriate animal data class.

4. We define a function `printAnimalInfo` that takes an `Animal` instance as an argument and prints its name and sound.

5. In the `main` function, we create three `Animal` instances: a cat named "Kitty," a dog named "Fido," and a lion named "Simba." We use the `Animal.Companion.createAnimal` method to create these instances.

6. We call the `printAnimalInfo` function for each animal instance to print its name and sound.

This code demonstrates the use of sealed classes, data classes, companion objects, and factory methods in Kotlin. It allows us to create different types of animals and print information about them in a concise and organized manner.