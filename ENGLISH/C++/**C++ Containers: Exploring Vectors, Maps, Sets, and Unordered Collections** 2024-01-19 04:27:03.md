```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>

using namespace std;

// Define a custom comparator function for sorting pairs by their second element
struct CompareSecondElement {
    bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
        return a.second < b.second;
    }
};

int main() {
    // Create a vector of pairs of integers
    vector<pair<int, int>> v = {{1, 2}, {3, 4}, {5, 6}, {7, 8}, {9, 10}};

    // Sort the vector by the second element of each pair using the custom comparator
    sort(v.begin(), v.end(), CompareSecondElement());

    // Print the sorted vector
    for (auto& p : v) {
        cout << p.first << " " << p.second << endl;
    }

    // Create a map of strings to vectors of integers
    map<string, vector<int>> m;

    // Insert some key-value pairs into the map
    m["key1"] = {1, 2, 3};
    m["key2"] = {4, 5, 6};
    m["key3"] = {7, 8, 9};

    // Iterate over the map and print the keys and values
    for (auto& kv : m) {
        cout << kv.first << ": ";
        for (auto& i : kv.second) {
            cout << i << " ";
        }
        cout << endl;
    }

    // Create a set of strings
    set<string> s;

    // Insert some strings into the set
    s.insert("apple");
    s.insert("banana");
    s.insert("cherry");
    s.insert("durian");
    s.insert("elderberry");

    // Iterate over the set and print the elements
    for (auto& str : s) {
        cout << str << endl;
    }

    // Create an unordered map of integers to strings
    unordered_map<int, string> um;

    // Insert some key-value pairs into the unordered map
    um[1] = "one";
    um[2] = "two";
    um[3] = "three";
    um[4] = "four";
    um[5] = "five";

    // Iterate over the unordered map and print the keys and values
    for (auto& kv : um) {
        cout << kv.first << ": " << kv.second << endl;
    }

    // Create an unordered set of integers
    unordered_set<int> us;

    // Insert some integers into the unordered set
    us.insert(1);
    us.insert(2);
    us.insert(3);
    us.insert(4);
    us.insert(5);

    // Iterate over the unordered set and print the elements
    for (auto& i : us) {
        cout << i << endl;
    }

    return 0;
}
```

This code demonstrates the use of various STL containers in C++, including vectors, maps, sets, and unordered containers. It creates and manipulates these containers, performing operations such as sorting, insertion, and iteration. A brief explanation of each part of the code:

- **Vector of Pairs**: A vector is a dynamic array that can store elements of any type. In this case, a vector of pairs of integers is created to store key-value pairs.

- **Sorting**: The vector of pairs is sorted based on the second element of each pair using a custom comparator function. The `CompareSecondElement` struct defines the comparator function.

- **Map of Strings to Vectors of Integers**: A map is a collection of key-value pairs, where the keys are unique and the values can be of any type. In this case, a map is created to store strings as keys and vectors of integers as values.

- **Set of Strings**: A set is a collection of unique elements. In this case, a set of strings is created to store unique fruit names.

- **Unordered Map of Integers to Strings**: An unordered map is similar to a map, but it uses a hash table for faster lookup and insertion. In this case, an unordered map is created to store integers as keys and strings as values.

- **Unordered Set of Integers**: An unordered set is similar to a set, but it also uses a hash table for faster lookup and insertion. In this case, an unordered set of integers is created to store unique integers.