**Requirements:**

- Create a program that reads a text file and performs a variety of text analysis operations on it.
- The program should be able to calculate the total number of words, lines, and characters in the file.
- It should also be able to compute the number of occurrences of a given word or phrase in the file.
- Additionally, the program should be able to find and display the longest word and the shortest word in the file.

**Code:**

```typescript
import fs from "fs";
import { resolve } from "path";

// Define the main function
async function main() {
  // Read the input text file
  const textFilePath = resolve(__dirname, "input.txt");
  const text = await fs.promises.readFile(textFilePath, "utf8");

  // 1. Calculate the total number of words, lines, and characters in the file

  // Split the text into words
  const words = text.split(/\W+/);  // Split the text into words using a regular expression that matches non-word characters (\W+).

  // Calculate the total number of words
  const wordCount = words.length;

  // Split the text into lines
  const lines = text.split(/\r?\n/);  // Split the text into lines using a regular expression that matches carriage return (\r) and line feed (\n).

  // Calculate the total number of lines
  const lineCount = lines.length;

  // Calculate the total number of characters
  const characterCount = text.length;

  // 2. Compute the number of occurrences of a given word or phrase in the file

  // Get the word or phrase to search for from the user
  const searchTerm = prompt("Enter the word or phrase to search for:");

  // Create a regular expression to match the search term
  const regex = new RegExp(searchTerm, "gi");  // Create a regular expression object using the search term and the "global" and "ignore case" flags.

  // Find all occurrences of the search term in the text
  const occurrences = text.match(regex);

  // Calculate the number of occurrences
  const occurrenceCount = occurrences ? occurrences.length : 0;

  // 3. Find and display the longest and shortest words in the file

  // Set the initial values for the longest and shortest words
  let longestWord = "";
  let shortestWord = "";

  // Iterate over the words in the file
  for (const word of words) {
    if (word.length > longestWord.length) {
      longestWord = word;
    } else if (word.length < shortestWord.length || shortestWord === "") {
      shortestWord = word;
    }
  }

  // Display the results
  console.log("Total Words:", wordCount);
  console.log("Total Lines:", lineCount);
  console.log("Total Characters:", characterCount);
  console.log("Occurrences of", searchTerm + ":", occurrenceCount);
  console.log("Longest Word:", longestWord);
  console.log("Shortest Word:", shortestWord);
}

// Call the main function
main();
```

**Explanation:**

- The program starts by reading the input text file using the `fs.promises.readFile()` function.
- It then calculates the total number of words, lines, and characters in the file by splitting the text into words and lines using regular expressions.
- To compute the number of occurrences of a given word or phrase, the program creates a regular expression object using the search term and searches for the occurrences in the text using the `match()` method.
- It finds the longest and shortest words in the file by iterating over the words and updating the corresponding variables.
- Finally, the program displays the results to the console.