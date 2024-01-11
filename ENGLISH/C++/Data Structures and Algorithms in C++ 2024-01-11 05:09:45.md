```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <queue>
#include <stack>

using namespace std;

// Define a custom comparator function for sorting a pair of integers by their second element.
struct CompareBySecondElement {
  bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
    return a.second < b.second;
  }
};

// Define a custom hash function for hashing a pair of integers.
struct PairHash {
  size_t operator()(const pair<int, int>& p) const {
    return hash<int>()(p.first) ^ hash<int>()(p.second);
  }
};

int main() {
  // Create a vector of integers.
  vector<int> v = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

  // Sort the vector in ascending order.
  sort(v.begin(), v.end());

  // Print the sorted vector.
  cout << "Sorted vector: ";
  for (int x : v) {
    cout << x << " ";
  }
  cout << endl;

  // Create a map from integers to strings.
  map<int, string> m;
  m[1] = "one";
  m[2] = "two";
  m[3] = "three";

  // Insert a new key-value pair into the map.
  m.insert(make_pair(4, "four"));

  // Print the map.
  cout << "Map: " << endl;
  for (auto it = m.begin(); it != m.end(); ++it) {
    cout << it->first << " -> " << it->second << endl;
  }

  // Create a set of strings.
  set<string> s;
  s.insert("apple");
  s.insert("banana");
  s.insert("cherry");

  // Insert a new element into the set.
  s.insert("durian");

  // Print the set.
  cout << "Set: " << endl;
  for (string x : s) {
    cout << x << " ";
  }
  cout << endl;

  // Create an unordered map from strings to integers.
  unordered_map<string, int> um;
  um["apple"] = 1;
  um["banana"] = 2;
  um["cherry"] = 3;

  // Insert a new key-value pair into the unordered map.
  um.insert(make_pair("durian", 4));

  // Print the unordered map.
  cout << "Unordered map: " << endl;
  for (auto it = um.begin(); it != um.end(); ++it) {
    cout << it->first << " -> " << it->second << endl;
  }

  // Create an unordered set of integers.
  unordered_set<int> us;
  us.insert(1);
  us.insert(2);
  us.insert(3);

  // Insert a new element into the unordered set.
  us.insert(4);

  // Print the unordered set.
  cout << "Unordered set: " << endl;
  for (int x : us) {
    cout << x << " ";
  }
  cout << endl;

  // Create a priority queue of integers.
  priority_queue<int> pq;
  pq.push(1);
  pq.push(2);
  pq.push(3);

  // Insert a new element into the priority queue.
  pq.push(4);

  // Print the priority queue.
  cout << "Priority queue: ";
  while (!pq.empty()) {
    cout << pq.top() << " ";
    pq.pop();
  }
  cout << endl;

  // Create a stack of strings.
  stack<string> st;
  st.push("apple");
  st.push("banana");
  st.push("cherry");

  // Push a new element onto the stack.
  st.push("durian");

  // Print the stack.
  cout << "Stack: ";
  while (!st.empty()) {
    cout << st.top() << " ";
    st.pop();
  }
  cout << endl;

  return 0;
}
```

Explanation:

This code demonstrates the use of various data structures in C++. It creates and manipulates vectors, maps, sets, unordered maps, unordered sets, priority queues, and stacks. The code also includes a custom comparator function and a custom hash function.

Here is a brief explanation of each data structure and its operations:

* **Vector:** A dynamic array that can grow and shrink as needed. It supports random access to elements and efficient insertion and deletion at the end of the vector.

* **Map:** A data structure that stores key-value pairs. The keys are unique and the values can be of any type. Maps are sorted by the keys, which allows for efficient searching and retrieval of values.

* **Set:** A data structure that stores unique elements. Sets are unordered, meaning that the elements are not stored in any particular order. This allows for efficient insertion, deletion, and searching of elements.

* **Unordered Map:** A data structure that stores key-value pairs. Unlike maps, unordered maps are not sorted by the keys. This allows for faster insertion and deletion of elements, but searching for a particular key is slower.

* **Unordered Set:** A data structure that stores unique elements. Unordered sets are unordered, like regular sets, but they are implemented using a hash table, which allows for faster insertion, deletion, and searching of elements.

* **Priority Queue:** A data structure that stores elements in a sorted order. The elements are typically retrieved in order from highest to lowest priority. Priority queues are often used to implement algorithms such as Dijkstra's algorithm and Prim's algorithm.

* **Stack:** A data structure that follows the Last-In-First-Out (LIFO) principle. Elements are added and removed from the top of the stack. Stacks are often used to implement recursion and to keep track of function calls.

The code also includes a custom comparator function and a custom hash function. The comparator function is used to sort the elements of a pair of integers by their second element. The hash function is used to hash a pair of integers into a unique key.

Overall, this code demonstrates the use of various data structures in C++ and provides a good starting point for learning about these data structures and their applications.