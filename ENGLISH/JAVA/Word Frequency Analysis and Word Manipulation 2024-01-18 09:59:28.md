```java
// Import required libraries
import java.util.Scanner;
import java.util.ArrayList;
import java.util.Collections;

public class ComplexJavaCode {

    public static void main(String[] args) {

        // Create a Scanner object for user input
        Scanner input = new Scanner(System.in);

        // Create an ArrayList to store user input
        ArrayList<String> userInput = new ArrayList<>();

        // Prompt the user to enter multiple words
        System.out.println("Enter multiple words, separated by spaces:");

        // Read the user input and store it in the ArrayList
        String line = input.nextLine();
        Collections.addAll(userInput, line.split(" "));

        // Create a HashMap to store the frequency of each word
        HashMap<String, Integer> wordFrequency = new HashMap<>();

        // Iterate over the ArrayList and count the frequency of each word
        for (String word : userInput) {
            // Convert the word to lowercase to ignore case-sensitivity
            String lowerCaseWord = word.toLowerCase();

            // Get the current frequency of the word
            int currentFrequency = wordFrequency.getOrDefault(lowerCaseWord, 0);

            // Increment the frequency of the word
            wordFrequency.put(lowerCaseWord, currentFrequency + 1);
        }

        // Create a List to store the unique words and their frequencies
        List<Map.Entry<String, Integer>> uniqueWords = new ArrayList<>(wordFrequency.entrySet());

        // Sort the List by the frequency of the words in descending order
        Collections.sort(uniqueWords, (a, b) -> b.getValue() - a.getValue());

        // Print the unique words and their frequencies
        System.out.println("\nWord Frequency Analysis:");
        for (Map.Entry<String, Integer> entry : uniqueWords) {
            System.out.println(entry.getKey() + ": " + entry.getValue());
        }

        // Find the longest word and the shortest word
        String longestWord = "";
        String shortestWord = "";
        int maxWordLength = 0;
        int minWordLength = Integer.MAX_VALUE;

        for (String word : userInput) {
            int wordLength = word.length();
            if (wordLength > maxWordLength) {
                longestWord = word;
                maxWordLength = wordLength;
            }
            if (wordLength < minWordLength) {
                shortestWord = word;
                minWordLength = wordLength;
            }
        }

        // Print the longest word and the shortest word
        System.out.println("\nLongest Word: " + longestWord);
        System.out.println("Shortest Word: " + shortestWord);

        // Create a StringBuilder to store the result
        StringBuilder result = new StringBuilder();

        // Iterate over the ArrayList and reverse each word
        for (String word : userInput) {
            result.append(new StringBuilder(word).reverse().toString());
            result.append(" ");
        }

        // Print the reversed words
        System.out.println("\nReversed Words:");
        System.out.println(result.toString().trim());

    }
}
```

Explanation:

1. We start by importing necessary libraries, including `Scanner`, `ArrayList`, `Collections`, `HashMap`, `List`, and `Map.Entry`.

2. In the `main` method, we create a `Scanner` object to read user input.

3. We create an `ArrayList` called `userInput` to store the user's input words.

4. We prompt the user to enter multiple words separated by spaces and read the input using the `nextLine()` method.

5. We split the input into individual words using the `split()` method and store them in the `userInput` list.

6. We create a `HashMap` called `wordFrequency` to store the frequency of each word.

7. We iterate over the `userInput` list and count the frequency of each word, storing it in the `wordFrequency` map.

8. We create a `List` called `uniqueWords` to store the unique words and their frequencies.

9. We convert the keys (words) in the `wordFrequency` map to lowercase to ignore case-sensitivity.

10. We sort the `uniqueWords` list in descending order based on the frequency of the words.

11. We print the unique words and their frequencies using a `for-each` loop.

12. We find the longest word and shortest word in the `userInput` list.

13. We create a `StringBuilder` called `result` to store the reversed words.

14. We iterate over the `userInput` list and reverse each word using the `StringBuilder`'s `reverse()` method.

15. We append a space after each reversed word to maintain the original order.

16. We print the reversed words stored in the `result` string.

This code demonstrates various Java concepts such as input handling, data structures (ArrayList, HashMap, List), loops, conditional statements, and string manipulation. It also showcases how to find statistics about the input data (word frequency, longest/shortest words) and transform the data (reversing words).