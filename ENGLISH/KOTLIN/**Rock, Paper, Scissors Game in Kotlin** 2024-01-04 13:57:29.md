```kotlin
// Import essential Kotlin libraries
import java.util.Scanner
import java.lang.StringBuilder
import kotlin.random.Random

// Define the main function as the entry point of the program
fun main(args: Array<String>) {
    // Initialize a Scanner object to read input from the console
    val scanner = Scanner(System.`in`)

    // Create a list of possible computer choices for the game
    val computerChoices = listOf("rock", "paper", "scissors")

    // Keep track of the player's score and the computer's score
    var playerScore = 0
    var computerScore = 0

    // Main game loop
    while (true) {
        // Prompt the player to enter their choice
        print("Enter your choice (rock, paper, or scissors): ")
        val playerChoice = scanner.nextLine().toLowerCase()

        // Check if the player's choice is valid
        if (playerChoice !in computerChoices) {
            println("Invalid choice! Please enter 'rock', 'paper', or 'scissors'")
            continue
        }

        // Randomly generate the computer's choice
        val computerChoice = computerChoices[Random.nextInt(0, 3)]

        // Determine the winner of the round
        val winner = when {
            playerChoice == computerChoice -> "Tie"
            playerChoice == "rock" && computerChoice == "scissors" -> "Player"
            playerChoice == "paper" && computerChoice == "rock" -> "Player"
            playerChoice == "scissors" && computerChoice == "paper" -> "Player"
            else -> "Computer"
        }

        // Display the results of the round
        println("You chose $playerChoice. The computer chose $computerChoice.")
        println("The winner is $winner!")

        // Update the scores accordingly
        when (winner) {
            "Player" -> playerScore++
            "Computer" -> computerScore++
        }

        // Display the current scores
        println("Your score: $playerScore. Computer score: $computerScore.")

        // Prompt the player to continue or quit the game
        print("Do you want to continue playing (y/n)? ")
        val answer = scanner.nextLine().toLowerCase()

        // Check if the player wants to continue playing
        if (answer == "n") {
            break
        }
    }

    // Display the final scores and determine the overall winner
    println("Final score: You: $playerScore, Computer: $computerScore.")
    val overallWinner = if (playerScore > computerScore) "Player" else "Computer"
    println("The overall winner is $overallWinner!")

    // Thank the player for playing the game
    println("Thanks for playing!")
}
```

**Explanation of the Code:**

1. **Importing Libraries:**
   - We start by importing essential Kotlin libraries, including the `Scanner` library for reading input from the console and the `Random` library for generating random numbers.

2. **Defining Constants:**
   - We define a list of possible computer choices for the game: `rock`, `paper`, and `scissors`.

3. **Initializing Variables:**
   - We initialize variables to keep track of the player's score and the computer's score, both initially set to 0.

4. **Main Game Loop:**
   - The program enters a `while` loop, which represents the main game loop. The loop continues until the player chooses to quit the game.

5. **Prompting for Player Input:**
   - Inside the loop, we prompt the player to enter their choice of `rock`, `paper`, or `scissors` using `println()` and `nextLine()`.

6. **Validating Player Input:**
   - We check if the player's choice is a valid option from the list of computer choices. If not, we display an error message and prompt the player to enter a valid choice.

7. **Generating Computer's Choice:**
   - We use `Random.nextInt()` to randomly generate the computer's choice from the list of possible options.

8. **Determining the Round Winner:**
   - We use a `when` expression to determine the winner of the round based on the player's and computer's choices. It considers all possible outcomes, including ties and wins for both players and the computer.

9. **Displaying Results:**
   - We display the player's choice, the computer's choice, and the winner of the round using `println()`.

10. **Updating Scores:**
   - We update the player's score and the computer's score based on the winner of the round.

11. **Displaying Current Scores:**
    - We display the current scores of the player and the computer using `println()`.

12. **Prompting to Continue or Quit:**
    - We prompt the player to continue playing or quit the game by displaying a message and reading their input using `nextLine()`.

13. **Handling the Player's Choice:**
    - We check if the player's answer is "n" to exit the game or "y" to continue playing.

14. **Displaying Final Scores and Overall Winner:**
    - After the game loop ends, we display the final scores and determine the overall winner based on the player's and computer's scores.

15. **Thanking the Player:**
    - Finally, we thank the player for playing the game using `println()`.

This program simulates a simple game of rock, paper, scissors between a player and a computer, keeping track of scores and allowing the player to continue playing or quit at any time.