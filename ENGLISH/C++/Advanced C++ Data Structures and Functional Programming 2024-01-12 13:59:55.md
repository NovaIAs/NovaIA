```c++
#include <iostream>
#include <vector>
#include <map>
#include <set>
#include <utility>
#include <functional>

using namespace std;

// Define a custom hash function for a pair of integers
struct PairHash {
    size_t operator()(const pair<int, int> &p) const {
        return hash<int>()(p.first) ^ hash<int>()(p.second);
    }
};

// Define a comparator function for a pair of integers
struct PairComparator {
    bool operator()(const pair<int, int> &p1, const pair<int, int> &p2) const {
        return p1.first < p2.first || (p1.first == p2.first && p1.second < p2.second);
    }
};

// Main function
int main() {
    // Create a vector of integers
    vector<int> v = {1, 2, 3, 4, 5};

    // Create a map from integers to a vector of integers
    map<int, vector<int>> m;

    // Insert each element of the vector into the map, along with its index
    for (int i = 0; i < v.size(); i++) {
        m[v[i]].push_back(i);
    }

    // Print the map
    for (auto it = m.begin(); it != m.end(); it++) {
        cout << it->first << ": ";
        for (auto index : it->second) {
            cout << index << " ";
        }
        cout << endl;
    }

    // Create a set of unique integers
    set<int> s;

    // Insert the keys from the map into the set
    for (auto it = m.begin(); it != m.end(); it++) {
        s.insert(it->first);
    }

    // Print the set
    for (auto it = s.begin(); it != s.end(); it++) {
        cout << *it << " ";
    }
    cout << endl;

    // Create a map from a pair of integers to a string
    map<pair<int, int>, string, PairHash, PairComparator> m2;

    // Insert a few key-value pair into the map
    m2[make_pair(1, 2)] = "Hello";
    m2[make_pair(3, 4)] = "World";
    m2[make_pair(5, 6)] = "C++";

    // Print the map
    for (auto it = m2.begin(); it != m2.end(); it++) {
        cout << "(" << it->first.first << ", " << it->first.second << "): " << it->second << endl;
    }

    // Create a function that takes a vector of integers and returns a set of unique integers
    function<set<int>(vector<int>)> f = [](vector<int> v) {
        set<int> s;
        for (auto it = v.begin(); it != v.end(); it++) {
            s.insert(*it);
        }
        return s;
    };

    // Call the function and print the result
    set<int> result = f(v);
    for (auto it = result.begin(); it != result.end(); it++) {
        cout << *it << " ";
    }
    cout << endl;

    return 0;
}
```

Explanation:

This code demonstrates several advanced C++ features, including:

1. **Vector, Map, and Set:** It uses standard C++ data structures like vector, map, and set to store and manipulate data.

2. **Custom Hash Function and Comparator:** It defines a custom hash function and comparator for a pair of integers, which are used in the map and set implementations.

3. **Map with Vector of Integers:** It demonstrates the use of a map to store integers as keys and a vector of integers as values.

4. **Set of Unique Integers from Map:** It extracts the keys from the map and inserts them into a set to get a set of unique integers.

5. **Map with Pair of Integers as Key and String as Value:** It uses a map to store a pair of integers as a key and a string as a value.

6. **Function that Takes a Vector and Returns a Set:** It defines a function that takes a vector of integers and returns a set of unique integers.

7. **Calling the Function and Printing the Result:** It calls the function with the input vector and then iterates through the resulting set and print the values.

This code showcases how C++ can be used to solve complex data manipulation problems with its built-in data structures and functional features.