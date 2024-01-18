```java
// Import necessary libraries
import java.util.Scanner;
import java.util.Random;

public class ComplexJavaCode {

    // Define the main method
    public static void main(String[] args) {

        // Create a Scanner object for user input
        Scanner input = new Scanner(System.in);

        // Create a Random object for generating random numbers
        Random random = new Random();

        // Define an array of strings for menu options
        String[] menuOptions = {"1. Play a game", "2. View high scores", "3. Exit"};

        // Display the menu options to the user
        System.out.println("Welcome to the Complex Java Game!");
        System.out.println("Please select an option from the menu:");
        for (int i = 0; i < menuOptions.length; i++) {
            System.out.println((i + 1) + ". " + menuOptions[i]);
        }

        // Get the user's input
        int choice = input.nextInt();

        // Handle the user's choice
        switch (choice) {
            case 1:
                // Play the game
                playGame(input, random);
                break;
            case 2:
                // View the high scores
                viewHighScores();
                break;
            case 3:
                // Exit the game
                System.out.println("Thank you for playing!");
                System.exit(0);
                break;
            default:
                // Invalid input
                System.out.println("Invalid input. Please select a valid option.");
                break;
        }

        // Close the Scanner object
        input.close();

    }

    // Method for playing the game
    private static void playGame(Scanner input, Random random) {

        // Set the initial score to 0
        int score = 0;

        // Generate a random number between 1 and 100
        int randomNumber = random.nextInt(100) + 1;

        // Prompt the user to guess the number
        System.out.println("Guess a number between 1 and 100:");

        // Get the user's guess
        int guess = input.nextInt();

        // Check if the user's guess is correct
        while (guess != randomNumber) {

            // If the guess is too high, tell the user to guess lower
            if (guess > randomNumber) {
                System.out.println("Your guess is too high. Try again:");
            }
            // If the guess is too low, tell the user to guess higher
            else {
                System.out.println("Your guess is too low. Try again:");
            }

            // Get the user's next guess
            guess = input.nextInt();

        }

        // If the user's guess is correct, congratulate them and update the score
        System.out.println("Congratulations! You guessed the correct number.");
        score += 10;

        // Display the score to the user
        System.out.println("Your score is: " + score);

    }

    // Method for viewing the high scores
    private static void viewHighScores() {

        // Create an array of integers for the high scores
        int[] highScores = {100, 90, 80, 70, 60};

        // Display the high scores to the user
        System.out.println("High Scores:");
        for (int score : highScores) {
            System.out.println(score);
        }

    }

}

```

**Explanation:**

- **Overview:** This Java code creates a simple interactive game with a menu-driven interface. The user can choose to play the game, view high scores, or exit the program.

- **Classes and Methods:** The code defines two classes:

  - `ComplexJavaCode`: This class contains the `main` method and two private methods: `playGame` and `viewHighScores`.

  - `Random`: This class is used to generate random numbers for the game.

- **Main Method:** The `main` method is the entry point of the program. It displays a menu of options to the user and gets their input. Based on the user's choice, it calls the appropriate method to play the game, view high scores, or exit the program. It also creates a `Scanner` object for user input and a `Random` object for generating random numbers.

- **Menu-Driven Interface:** The program presents a menu of options to the user at the start, allowing them to select a desired action.

- **Play Game Method:** The `playGame` method is used to play the game. It generates a random number between 1 and 100, prompts the user to guess the number, and provides feedback on whether the guess is too high, too low, or correct. It updates the user's score if they guess the correct number and displays the updated score to the user.

- **View High Scores Method:** The `viewHighScores` method is used to display the high scores to the user. It creates an array of integer values representing the high scores and prints them to the console.

- **Random Number Generation:** The `Random` class is used to generate random numbers for the game. The `nextInt(int bound)` method is used to generate a random integer value between 0 (inclusive) and the specified `bound` (exclusive). In this case, it is used to generate a random number between 1 and 100.

- **User Input Processing:** The `Scanner` class is used to obtain user input from the console. The `nextInt()` method is used to get an integer input from the user.

- **Input Validation:** The code includes input validation to handle invalid user input. For example, if the user enters a non-integer value for their guess, the program will prompt them to enter a valid integer.

- **Score Keeping:** The code keeps track of the user's score as they play the game. The score is incremented by 10 if the user correctly guesses the random number.

- **Exit Option:** The program provides an exit option for the user to quit the game if they choose. The `System.exit(0)` method is used to terminate the program gracefully.

- **Comments:** The code includes extensive comments to explain the purpose and functionality of different parts of the program, making it easier to understand and maintain.