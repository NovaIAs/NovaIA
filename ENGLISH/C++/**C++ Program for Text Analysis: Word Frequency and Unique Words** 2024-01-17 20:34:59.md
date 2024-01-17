```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>
#include <sstream>

using namespace std;

// Define a custom comparator for sorting pairs by their second element.
struct comparePairs {
  bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
    return a.second < b.second;
  }
};

// Define a custom function for splitting a string into a vector of strings.
vector<string> split(const string& str, char delimiter) {
  vector<string> tokens;
  stringstream ss(str);
  string token;

  while (getline(ss, token, delimiter)) {
    tokens.push_back(token);
  }

  return tokens;
}

// Define the main function.
int main() {
  // Create a vector of strings.
  vector<string> words = {"apple", "banana", "cherry", "dog", "elephant", "fish"};

  // Create a map to store the frequency of each word.
  map<string, int> wordFrequency;

  // Iterate over the vector of words and update the frequency map.
  for (const string& word : words) {
    wordFrequency[word]++;
  }

  // Create a set to store the unique words.
  set<string> uniqueWords;

  // Iterate over the frequency map and add the unique words to the set.
  for (const auto& [word, frequency] : wordFrequency) {
    uniqueWords.insert(word);
  }

  // Create a vector of pairs to store the word-frequency pairs.
  vector<pair<string, int>> wordFrequencyPairs;

  // Iterate over the frequency map and add the word-frequency pairs to the vector.
  for (const auto& [word, frequency] : wordFrequency) {
    wordFrequencyPairs.push_back(make_pair(word, frequency));
  }

  // Sort the vector of word-frequency pairs by the frequency in descending order.
  sort(wordFrequencyPairs.begin(), wordFrequencyPairs.end(), comparePairs());

  // Print the top 5 most frequent words.
  cout << "Top 5 Most Frequent Words:" << endl;
  for (int i = 0; i < 5; i++) {
    cout << wordFrequencyPairs[i].first << " (" << wordFrequencyPairs[i].second << ")" << endl;
  }

  // Print the unique words.
  cout << "Unique Words:" << endl;
  for (const string& word : uniqueWords) {
    cout << word << endl;
  }

  return 0;
}
```

**Explanation:**

This C++ program performs various text analysis tasks on a given vector of words. Here's a breakdown of what the code does:

1. **Input Vector of Words:**
   - A vector of strings called `words` is created to store the input words. This vector contains words like "apple," "banana," "cherry," and so on.

2. **Frequency Map:**
   - A map called `wordFrequency` is created to store the frequency of each word in the input vector.
   - The program iterates over the `words` vector and updates the `wordFrequency` map accordingly.

3. **Set of Unique Words:**
   - A set called `uniqueWords` is created to store the unique words from the input vector.
   - The program iterates over the `wordFrequency` map and adds the unique words to the `uniqueWords` set.

4. **Vector of Word-Frequency Pairs:**
   - A vector called `wordFrequencyPairs` is created to store pairs consisting of word-frequency pairs.
   - The program iterates over the `wordFrequency` map and adds the word-frequency pairs to the `wordFrequencyPairs` vector.

5. **Sorting Word-Frequency Pairs:**
   - The `wordFrequencyPairs` vector is sorted in descending order based on the frequency of each word.
   - This is done using the custom `comparePairs` struct, which defines the comparison criteria.

6. **Printing Top 5 Most Frequent Words:**
   - The program prints the top 5 most frequent words from the sorted `wordFrequencyPairs` vector.

7. **Printing Unique Words:**
   - The program prints the unique words stored in the `uniqueWords` set.

This program essentially analyzes the input vector of words, calculates the frequency of each word, identifies unique words, and presents the results in a structured manner. It demonstrates the use of various data structures and algorithms in C++.