```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <cmath>

using namespace std;

// Define a custom comparator function for the priority queue
struct Compare {
  bool operator()(pair<int, int> a, pair<int, int> b) {
    return a.first < b.first; // Reverse the order for a min-heap
  }
};

// Define a custom hash function for the unordered_map
struct HashPair {
  size_t operator()(const pair<int, int>& p) const {
    return hash<int>()(p.first) ^ hash<int>()(p.second);
  }
};

int main() {
  // Create a priority queue with a custom comparator (max-heap)
  priority_queue<pair<int, int>, vector<pair<int, int>>, Compare> pq;

  // Insert some values into the priority queue
  pq.push(make_pair(10, 20));
  pq.push(make_pair(5, 15));
  pq.push(make_pair(15, 25));

  // Print the top element of the priority queue
  cout << "Top element: (" << pq.top().first << ", " << pq.top().second << ")\n";

  // Create an unordered_map with a custom hash function
  unordered_map<pair<int, int>, int, HashPair> m;

  // Insert some key-value pairs into the unordered_map
  m[make_pair(1, 2)] = 10;
  m[make_pair(3, 4)] = 20;
  m[make_pair(5, 6)] = 30;

  // Find a specific key in the unordered_map
  if (m.find(make_pair(3, 4)) != m.end()) {
    cout << "Key found!\n";
  } else {
    cout << "Key not found!\n";
  }

  // Create a set of pairs
  set<pair<int, int>> s;

  // Insert some pairs into the set
  s.insert(make_pair(1, 2));
  s.insert(make_pair(3, 4));
  s.insert(make_pair(5, 6));

  // Check if a specific pair exists in the set
  if (s.find(make_pair(3, 4)) != s.end()) {
    cout << "Pair found!\n";
  } else {
    cout << "Pair not found!\n";
  }

  // Create a vector of vectors
  vector<vector<int>> vv;

  // Initialize the vector of vectors
  for (int i = 0; i < 3; i++) {
    vector<int> v;
    for (int j = 0; j < 4; j++) {
      v.push_back(i * j);
    }
    vv.push_back(v);
  }

  // Print the vector of vectors
  for (int i = 0; i < vv.size(); i++) {
    for (int j = 0; j < vv[i].size(); j++) {
      cout << vv[i][j] << " ";
    }
    cout << endl;
  }

  // Create a map of vectors
  map<int, vector<int>> mv;

  // Initialize the map of vectors
  for (int i = 0; i < 3; i++) {
    vector<int> v;
    for (int j = 0; j < 4; j++) {
      v.push_back(i * j);
    }
    mv[i] = v;
  }

  // Print the map of vectors
  for (auto it = mv.begin(); it != mv.end(); it++) {
    cout << it->first << ": ";
    for (auto vit = it->second.begin(); vit != it->second.end(); vit++) {
      cout << *vit << " ";
    }
    cout << endl;
  }

  // Calculate the square root of a number
  double num = 25.0;
  double sqrt_num = sqrt(num);
  cout << "Square root of " << num << " is " << sqrt_num << endl;

  // Return 0 to indicate successful execution
  return 0;
}
```

**Explanation:**

The provided code demonstrates various advanced features and data structures in C++, including the use of priority queues, unordered maps, sets, vectors of vectors, maps of vectors, and mathematical calculations. A breakdown of the code:

1. **Priority Queue:** A priority queue is used to implement a max-heap, where the element with the highest priority (in this case, the smallest integer value) is at the top. This is achieved by defining a custom comparator function and passing it as a template argument to the priority queue.

2. **Unordered Map:** An unordered map is used to map key-value pairs, in this case, pairs of integers. A custom hash function is defined to efficiently calculate the hash value for each key, which is a pair of integers.

3. **Set:** A set is used to store a collection of unique elements, in this case, pairs of integers. The set automatically sorts and maintains the uniqueness of the elements.

4. **Vector of Vectors:** A vector of vectors is a dynamic array of dynamic arrays. It allows you to store a collection of vectors, each of which can have a different size.

5. **Map of Vectors:** A map of vectors is a data structure that allows you to map keys (in this case, integers) to vectors of values. Each vector can have a different size and contains integers.

6. **Mathematical Calculations:** The code demonstrates the use of the mathematical function `sqrt` to calculate the square root of a number. It prints the square root of the number 25.0.

This code covers a wide range of advanced data structures and techniques in C++, making it quite comprehensive and complex.