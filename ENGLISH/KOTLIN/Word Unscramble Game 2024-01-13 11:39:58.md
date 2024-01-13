```kotlin
// Import the necessary libraries.
import java.util.Scanner
import java.util.Random

// Define the main function.
fun main(args: Array<String>) {
    // Create a Scanner object to read input from the console.
    val scanner = Scanner(System.`in`)

    // Create a Random object to generate random numbers.
    val random = Random()

    // Create a list of words.
    val words = listOf("hello", "world", "computer", "science", "programming")

    // Get a random word from the list.
    val word = words[random.nextInt(words.size)]

    // Create a list of characters.
    val characters = word.toCharArray()

    // Shuffle the characters in the list.
    characters.shuffle()

    // Create a new string from the shuffled characters.
    val shuffledWord = String(characters)

    // Print the shuffled word to the console.
    println("Scrambled word: $shuffledWord")

    // Get the user's guess for the word.
    println("Guess the word:")
    val guess = scanner.nextLine()

    // Check if the user's guess is correct.
    if (guess == word) {
        println("Congratulations! You guessed the word correctly.")
    } else {
        println("Sorry, your guess was incorrect. The correct word is $word.")
    }
}
```

**Explanation:**

This Kotlin code is a simple word game where the user has to guess a scrambled word. The code first imports the necessary libraries and then defines the `main` function, which is the entry point of the program.

Inside the `main` function, a `Scanner` object is created to read input from the console and a `Random` object is created to generate random numbers. A list of words is then defined and a random word is selected from the list. The characters in the random word are shuffled and a new string is created from the shuffled characters. The shuffled word is then printed to the console and the user is prompted to guess the word.

The user's guess is then read from the console and compared to the original word. If the guess is correct, a congratulatory message is printed to the console. Otherwise, a message is printed to the console indicating that the guess was incorrect and the correct word is revealed.