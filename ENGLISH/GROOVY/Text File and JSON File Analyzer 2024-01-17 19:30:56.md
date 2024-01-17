```groovy
import groovy.json.JsonSlurper
import java.nio.file.Files
import java.nio.file.Paths

// Define a method to calculate the number of occurrences of a given word in a text file
def countWordOccurrences(String filePath, String word) {
    // Read the file contents into a string
    String fileContents = Files.readString(Paths.get(filePath))

    // Convert the string to lowercase and split it into an array of words
    String[] words = fileContents.toLowerCase().split("\\W+")

    // Count the number of occurrences of the given word in the array
    int count = 0
    for (String w : words) {
        if (w == word) {
            count++
        }
    }

    // Return the count
    return count
}

// Define a method to find the most frequently occurring words in a text file
def findMostFrequentWords(String filePath, int numWords) {
    // Read the file contents into a string
    String fileContents = Files.readString(Paths.get(filePath))

    // Convert the string to lowercase and split it into an array of words
    String[] words = fileContents.toLowerCase().split("\\W+")

    // Create a map to store the word frequencies
    Map<String, Integer> wordFrequencies = new HashMap<>()

    // Count the frequency of each word in the array
    for (String w : words) {
        if (wordFrequencies.containsKey(w)) {
            wordFrequencies[w]++
        } else {
            wordFrequencies[w] = 1
        }
    }

    // Sort the map by word frequency in descending order
    Map<String, Integer> sortedWordFrequencies = wordFrequencies.sort { it.value <=> it.key }

    // Return the top numWords most frequently occurring words
    return sortedWordFrequencies.take(numWords)
}

// Define a method to extract data from a JSON file
def extractDataFromJson(String filePath) {
    // Read the file contents into a string
    String fileContents = Files.readString(Paths.get(filePath))

    // Parse the JSON string into a map
    Map<String, Object> data = new JsonSlurper().parseText(fileContents)

    // Return the data map
    return data
}

// Define a method to print the results of a calculation
def printResults(String message, Object results) {
    println message
    println "===================================================="
    println results
    println ""
}

// Get the file path to the text file
String filePath = "path/to/text_file.txt"

// Count the number of occurrences of the word "Java" in the text file
int javaCount = countWordOccurrences(filePath, "Java")

// Find the 5 most frequently occurring words in the text file
List<Map.Entry<String, Integer>> mostFrequentWords = findMostFrequentWords(filePath, 5)

// Extract data from the JSON file
Map<String, Object> jsonData = extractDataFromJson("path/to/json_file.json")

// Print the results
printResults("Number of occurrences of the word \"Java\" in the text file:", javaCount)
printResults("Top 5 most frequently occurring words in the text file:", mostFrequentWords)
printResults("Data extracted from the JSON file:", jsonData)
```

In this code:

- The `countWordOccurrences` method counts the number of occurrences of a given word in a text file.
- The `findMostFrequentWords` method finds the most frequently occurring words in a text file.
- The `extractDataFromJson` method extracts data from a JSON file.
- The `printResults` method prints the results of a calculation.

The code also includes a few helper methods for reading files and parsing JSON strings.

The code is used to analyze a text file and a JSON file, and then print the results of the analysis. The output of the code will look something like this:

```
Number of occurrences of the word "Java" in the text file:
5

Top 5 most frequently occurring words in the text file:
[
    (the, 10),
    (is, 9),
    (of, 8),
    (and, 7),
    (a, 6)
]

Data extracted from the JSON file:
{
    "name": "John Doe",
    "age": 30,
    "city": "New York",
    "occupation": "Software Engineer"
}
```