```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <string>

// Define a custom comparator for sorting pairs of integers by their second element.
struct CompareSecond {
  bool operator()(const std::pair<int, int>& a, const std::pair<int, int>& b) {
    return a.second < b.second;
  }
};

// Define a function to print the elements of a vector of integers.
void printVector(const std::vector<int>& v) {
  for (int i = 0; i < v.size(); i++) {
    std::cout << v[i] << " ";
  }
  std::cout << std::endl;
}

// Define a function to print the elements of a map of strings to integers.
void printMap(const std::map<std::string, int>& m) {
  for (auto it = m.begin(); it != m.end(); it++) {
    std::cout << it->first << " -> " << it->second << std::endl;
  }
}

// Define a function to print the elements of a set of strings.
void printSet(const std::set<std::string>& s) {
  for (auto it = s.begin(); it != s.end(); it++) {
    std::cout << *it << " ";
  }
  std::cout << std::endl;
}

// Define the main function.
int main() {
  // Create a vector of integers.
  std::vector<int> v = {1, 3, 5, 2, 4, 6};

  // Sort the vector in ascending order using the default comparison operator.
  std::sort(v.begin(), v.end());
  std::cout << "Sorted vector in ascending order: ";
  printVector(v);

  // Sort the vector in descending order using a custom comparator.
  std::sort(v.begin(), v.end(), CompareSecond());
  std::cout << "Sorted vector in descending order: ";
  printVector(v);

  // Create a map of strings to integers.
  std::map<std::string, int> m = {{"apple", 1}, {"banana", 2}, {"cherry", 3}};

  // Print the elements of the map.
  std::cout << "Map of strings to integers:" << std::endl;
  printMap(m);

  // Create a set of strings.
  std::set<std::string> s = {"apple", "banana", "cherry"};

  // Print the elements of the set.
  std::cout << "Set of strings:" << std::endl;
  printSet(s);

  // Find an element in the set.
  auto it = s.find("banana");
  if (it != s.end()) {
    std::cout << "Found \"banana\" in the set" << std::endl;
  } else {
    std::cout << "\"banana\" not found in the set" << std::endl;
  }

  // Erase an element from the set.
  s.erase("cherry");
  std::cout << "Set after erasing \"cherry\": ";
  printSet(s);

  return 0;
}
```

This C++ code demonstrates various data structures and operations on them, including sorting a vector of integers, printing the elements of a map of strings to integers, searching for an element in a set of strings, and erasing an element from a set.

Here's a detailed explanation of the code:

1. **Sorting a Vector of Integers:**

   - We define a vector of integers `v` containing the values `{1, 3, 5, 2, 4, 6}`.
   - We use `std::sort()` to sort the vector in ascending order using the default comparison operator.
   - We then sort the vector in descending order using a custom comparator `CompareSecond()`, which compares pairs of integers based on their second element.

2. **Printing the Elements of a Map:**

   - We create a map of strings to integers `m`, with key-value pairs such as `{"apple", 1}, {"banana", 2}, {"cherry", 3}`.
   - We use a range-based `for` loop to iterate over the map and print each key-value pair.

3. **Searching for an Element in a Set:**

   - We create a set of strings `s` containing the values `{"apple", "banana", "cherry"}`.
   - We use `std::find()` to search for the element "banana" in the set.
   - If the element is found, we print a message indicating that it was found; otherwise, we print a message indicating that it was not found.

4. **Erasing an Element from a Set:**

   - We use `std::erase()` to erase the element "cherry" from the set `s`.
   - We then print the elements of the set to show that "cherry" has been removed.

This code demonstrates various operations on different data structures and provides a comprehensive example of how to work with them in C++.