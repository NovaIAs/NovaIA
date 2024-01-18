```dart
// Import necessary libraries
import 'dart:io';
import 'dart:math';
import 'dart:collection';

void main() {
  // Create a random number generator
  final random = Random();

  // Create a hash map to store word frequencies
  final wordFrequencies = HashMap<String, int>();

  // Read the input text file
  final inputFile = File('input.txt');
  final inputText = inputFile.readAsStringSync();

  // Split the input text into words
  final words = inputText.split(' ');

  // Calculate the word frequencies
  for (final word in words) {
    final lowerCaseWord = word.toLowerCase();
    if (wordFrequencies.containsKey(lowerCaseWord)) {
      wordFrequencies[lowerCaseWord] = wordFrequencies[lowerCaseWord]! + 1;
    } else {
      wordFrequencies[lowerCaseWord] = 1;
    }
  }

  // Sort the word frequencies in descending order
  final sortedWordFrequencies = wordFrequencies.entries
      .toList()
      ..sort((a, b) => b.value.compareTo(a.value));

  // Print the top 10 most frequent words
  print('Top 10 Most Frequent Words:');
  for (var i = 0; i < 10; i++) {
    final wordFrequency = sortedWordFrequencies[i];
    print('${wordFrequency.key}: ${wordFrequency.value}');
  }

  // Create a histogram of the word frequencies
  final histogram = List.filled(10, 0);
  for (final wordFrequency in sortedWordFrequencies) {
    final frequency = wordFrequency.value;
    final index = (frequency / 10).floor();
    histogram[index]++;
  }

  // Print the histogram
  print('\nHistogram of Word Frequencies:');
  for (var i = 0; i < histogram.length; i++) {
    print('${i * 10}-${(i + 1) * 10 - 1}: ${'*' * histogram[i]}');
  }

  // Generate a random sentence using the top 10 most frequent words
  final sentence = List.filled(10, '');
  for (var i = 0; i < 10; i++) {
    final wordFrequency = sortedWordFrequencies[i];
    sentence[i] = wordFrequency.key;
  }

  // Shuffle the sentence
  sentence.shuffle(random);

  // Print the generated sentence
  print('\nGenerated Sentence:');
  print(sentence.join(' '));
}
```

**Explanation:**

This Dart program performs a detailed analysis of word frequencies in a text file and generates a histogram and a random sentence using the top 10 most frequent words:

1. **Import Libraries:** The program imports necessary libraries for handling files, generating random numbers, and working with collections.

2. **Create Random Number Generator:** A random number generator is created to shuffle the generated sentence later.

3. **Create HashMap for Word Frequencies:** A hash map is created to store word frequencies. It maps each unique word (in lowercase) to its frequency of occurrence in the text.

4. **Read Input Text File:** The input text file is read and stored as a string.

5. **Split Text into Words:** The input text is split into individual words using the `split()` method.

6. **Calculate Word Frequencies:** Each word is converted to lowercase, and its frequency is incremented in the `wordFrequencies` hash map.

7. **Sort Word Frequencies:** The word frequencies are sorted in descending order based on their values.

8. **Print Top 10 Most Frequent Words:** The top 10 most frequent words are printed along with their frequencies.

9. **Create Histogram of Word Frequencies:** A histogram is created to visualize the distribution of word frequencies. Each bin in the histogram represents a range of frequencies, and the number of asterisks in each bin represents the number of words in that range.

10. **Generate Random Sentence:** A random sentence is generated using the top 10 most frequent words. The words are shuffled to create a unique sentence.

11. **Print Generated Sentence:** The generated sentence is printed.