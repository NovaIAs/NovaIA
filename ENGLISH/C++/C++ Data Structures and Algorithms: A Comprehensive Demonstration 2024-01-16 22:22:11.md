```c++
// Header Files
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <cmath>
#include <iomanip>

// Using Namespace
using namespace std;

// Function Templates
template <typename T>
void printVector(vector<T> v) {
  for (auto &i : v) {
    cout << i << " ";
  }
  cout << endl;
}

template <typename T>
void printMap(map<T, T> m) {
  for (auto &i : m) {
    cout << i.first << " -> " << i.second << endl;
  }
}

template <typename T>
void printSet(set<T> s) {
  for (auto &i : s) {
    cout << i << " ";
  }
  cout << endl;
}

// Main Function
int main() {
  // Vector of Integers
  vector<int> v1 = {1, 2, 3, 4, 5};
  printVector(v1);  // Output: 1 2 3 4 5

  // Vector of Strings
  vector<string> v2 = {"Hello", "World", "!"};
  printVector(v2);  // Output: Hello World !

  // Map of Integers to Strings
  map<int, string> m1 = {{1, "One"}, {2, "Two"}, {3, "Three"}};
  printMap(m1);  // Output: 1 -> One, 2 -> Two, 3 -> Three

  // Set of Doubles
  set<double> s1 = {1.2, 3.4, 5.6, 7.8, 9.0};
  printSet(s1);  // Output: 1.2 3.4 5.6 7.8 9.0

  // Queue of Characters
  queue<char> q1;
  q1.push('a');
  q1.push('b');
  q1.push('c');
  while (!q1.empty()) {
    cout << q1.front() << " ";  // Output: a b c
    q1.pop();
  }
  cout << endl;

  // Stack of Integers
  stack<int> s2;
  s2.push(1);
  s2.push(2);
  s2.push(3);
  while (!s2.empty()) {
    cout << s2.top() << " ";  // Output: 3 2 1
    s2.pop();
  }
  cout << endl;

  // Sorting a Vector of Strings
  vector<string> v3 = {"Apple", "Banana", "Cherry", "Date"};
  sort(v3.begin(), v3.end());
  printVector(v3);  // Output: Apple Banana Cherry Date

  // Finding Maximum in a Set of Integers
  set<int> s3 = {1, 3, 5, 7, 9};
  cout << *s3.rbegin() << endl;  // Output: 9

  // Calculating Factorial of a Number
  int n = 5;
  int factorial = 1;
  for (int i = 1; i <= n; i++) {
    factorial *= i;
  }
  cout << factorial << endl;  // Output: 120

  return 0;
}
```

Explanation:

1. We include the necessary header files for input/output operations, data structures, and mathematical functions.

2. We define two function templates, `printVector()`, `printMap()`, `printSet()`, to print the contents of vectors, maps, and sets, respectively.

3. In the `main()` function:
   - We create and print a vector of integers.
   - We create and print a vector of strings.
   - We create and print a map of integers to strings.
   - We create and print a set of doubles.
   - We create a queue of characters, enqueue some characters, dequeue and print them.
   - We create a stack of integers, push some integers, pop and print them.
   - We sort a vector of strings and print the sorted vector.
   - We find the maximum element in a set of integers and print it.
   - We calculate the factorial of a number and print it.

This code demonstrates various data structures, operations, and algorithms commonly used in C++ programming. It also showcases the use of function templates for generic programming.