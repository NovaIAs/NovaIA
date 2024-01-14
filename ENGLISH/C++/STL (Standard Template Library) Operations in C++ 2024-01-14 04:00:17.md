```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <stack>
#include <queue>
#include <unordered_map>
#include <unordered_set>
#include <string>
#include <sstream>
#include <fstream>

using namespace std;

// Define a custom comparator function for sorting strings by their length
bool compareStringLength(const string& a, const string& b) {
  return a.length() < b.length();
}

// Main function
int main() {
  // Create a vector of strings
  vector<string> strings = {"apple", "banana", "cherry", "durian", "elderberry", "fig"};

  // Sort the vector of strings using the custom comparator function
  sort(strings.begin(), strings.end(), compareStringLength);

  // Print the sorted vector of strings
  for (const string& str : strings) {
    cout << str << " ";
  }
  cout << endl;

  // Create a map of strings to integers
  map<string, int> stringToIntMap;
  stringToIntMap["apple"] = 1;
  stringToIntMap["banana"] = 2;
  stringToIntMap["cherry"] = 3;

  // Iterate over the map and print the key-value pairs
  for (const auto& pair : stringToIntMap) {
    cout << pair.first << ": " << pair.second << endl;
  }

  // Create a set of integers
  set<int> integers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Find the maximum element in the set
  int maxElement = *integers.rbegin();

  // Print the maximum element
  cout << "Maximum element in the set: " << maxElement << endl;

  // Create a stack of characters
  stack<char> characters;
  characters.push('a');
  characters.push('b');
  characters.push('c');

  // Pop and print the top element of the stack
  char topElement = characters.top();
  characters.pop();
  cout << "Top element of the stack: " << topElement << endl;

  // Create a queue of integers
  queue<int> numbers;
  numbers.push(1);
  numbers.push(2);
  numbers.push(3);

  // Dequeue and print the front element of the queue
  int frontElement = numbers.front();
  numbers.pop();
  cout << "Front element of the queue: " << frontElement << endl;

  // Create an unordered map of strings to integers
  unordered_map<string, int> unorderedStringToIntMap;
  unorderedStringToIntMap["apple"] = 1;
  unorderedStringToIntMap["banana"] = 2;
  unorderedStringToIntMap["cherry"] = 3;

  // Find the value associated with the key "cherry"
  int cherryValue = unorderedStringToIntMap["cherry"];

  // Print the value
  cout << "Value associated with the key \"cherry\": " << cherryValue << endl;

  // Create an unordered set of integers
  unordered_set<int> unorderedIntegers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Check if the element 5 is present in the set
  bool isPresent = unorderedIntegers.count(5);

  // Print the result
  cout << "Is element 5 present in the set? " << (isPresent ? "Yes" : "No") << endl;

  // Read a line of text from the console
  string line;
  cout << "Enter a line of text: ";
  getline(cin, line);

  // Tokenize the line of text into words
  vector<string> words;
  stringstream ss(line);
  string word;
  while (ss >> word) {
    words.push_back(word);
  }

  // Print the words
  cout << "Words in the line of text: ";
  for (const string& word : words) {
    cout << word << " ";
  }
  cout << endl;

  // Open a file for writing
  ofstream file;
  file.open("output.txt");

  // Write some data to the file
  file << "Hello, world!" << endl;
  file << "This is a line of text written to a file." << endl;

  // Close the file
  file.close();

  return 0;
}
```

This code demonstrates the use of various STL (Standard Template Library) containers and algorithms in C++. It includes examples of sorting a vector of strings using a custom comparator, creating a map of strings to integers, finding the maximum element in a set, using a stack and a queue, working with unordered maps and sets, reading input from the console, tokenizing a line of text, and writing data to a file.