```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <functional>

using namespace std;

// Define a custom comparator function to sort strings by their length
bool sortByLength(const string& a, const string& b) {
  return a.length() < b.length();
}

// Define a custom function object to print strings with a specific prefix
struct PrintWithPrefix {
  string prefix;

  PrintWithPrefix(const string& prefix) : prefix(prefix) {}

  void operator()(const string& str) {
    cout << prefix << str << endl;
  }
};

int main() {
  // Create a vector of strings
  vector<string> strings = {"apple", "banana", "cherry", "durian", "elderberry"};

  // Sort the strings by their length using the custom comparator function
  sort(strings.begin(), strings.end(), sortByLength);

  // Print the sorted strings with the prefix "Sorted by length: " using the custom function object
  for_each(strings.begin(), strings.end(), PrintWithPrefix("Sorted by length: "));

  // Reverse the order of the strings in the vector
  reverse(strings.begin(), strings.end());

  // Print the reversed strings with the prefix "Reversed: " using a lambda function
  for_each(strings.begin(), strings.end(), [](const string& str) {
    cout << "Reversed: " << str << endl;
  });

  // Find the longest string in the vector using the max_element function
  auto longest_string = max_element(strings.begin(), strings.end(), [](const string& a, const string& b) {
    return a.length() < b.length();
  });

  // Print the longest string with the prefix "Longest string: "
  cout << "Longest string: " << *longest_string << endl;

  // Find the shortest string in the vector using the min_element function
  auto shortest_string = min_element(strings.begin(), strings.end(), [](const string& a, const string& b) {
    return a.length() < b.length();
  });

  // Print the shortest string with the prefix "Shortest string: "
  cout << "Shortest string: " << *shortest_string << endl;

  return 0;
}
```

Explanation:

1. We define two custom functions:
   - `sortByLength`: A comparator function that sorts strings by their length.
   - `PrintWithPrefix`: A function object that prints strings with a specific prefix.

2. We create a vector of strings and initialize it with some values.

3. We sort the strings in the vector by their length using the `sort` function and the `sortByLength` comparator.

4. We print the sorted strings with the prefix "Sorted by length: " using the `for_each` function and the `PrintWithPrefix` function object.

5. We reverse the order of the strings in the vector using the `reverse` function.

6. We print the reversed strings with the prefix "Reversed: " using a lambda function passed to the `for_each` function.

7. We find the longest string in the vector using the `max_element` function and a lambda function as the comparison criterion.

8. We print the longest string with the prefix "Longest string: ".

9. We find the shortest string in the vector using the `min_element` function and a lambda function as the comparison criterion.

10. We print the shortest string with the prefix "Shortest string: ".