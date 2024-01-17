// Import necessary libraries
import Foundation

// Define a protocol called "Animal" with a required method called "makeNoise()"
protocol Animal {
    func makeNoise()
}

// Create a class called "Dog" that conforms to the "Animal" protocol
class Dog: Animal {
    // Implement the "makeNoise()" method for the "Dog" class
    func makeNoise() {
        print("Woof!")
    }
}

// Create a class called "Cat" that conforms to the "Animal" protocol
class Cat: Animal {
    // Implement the "makeNoise()" method for the "Cat" class
    func makeNoise() {
        print("Meow!")
    }
}

// Create a function called "makeAnimalsSpeak()" that takes an array of animals as its input
func makeAnimalsSpeak(animals: [Animal]) {
    // Iterate over the array of animals
    for animal in animals {
        // Call the "makeNoise()" method for each animal
        animal.makeNoise()
    }
}

// Create an array of animals
let animals: [Animal] = [Dog(), Cat()]

// Call the "makeAnimalsSpeak()" function with the array of animals
makeAnimalsSpeak(animals: animals)

// Explain the code:
// 1. We define a protocol called "Animal" with a required method called "makeNoise()". This protocol represents the common behavior of different types of animals making noise.
// 2. We create two classes called "Dog" and "Cat" that conform to the "Animal" protocol. Each class implements the "makeNoise()" method to specify the specific sound made by that animal.
// 3. We create a function called "makeAnimalsSpeak()" that takes an array of animals as its input. This function iterates over the array of animals and calls the "makeNoise()" method for each animal, causing each animal to make its specific sound.
// 4. We create an array of animals containing a dog and a cat.
// 5. We call the "makeAnimalsSpeak()" function with the array of animals, causing the dog and the cat to make their respective sounds.