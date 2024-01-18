```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

using namespace std;

// Define a custom comparator function for sorting pairs by their second element
struct SortBySecond {
    bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
        return a.second < b.second;
    }
};

// Define a custom hash function for integers
struct IntHash {
    size_t operator()(int key) const {
        return hash<int>()(key);
    }
};

// Define a custom equality comparison function for integers
struct IntEqual {
    bool operator()(int a, int b) const {
        return a == b;
    }
};

int main() {
    // Create a vector of integers
    vector<int> vec = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    // Create a map of integers to strings using a custom hash function and equality comparison function
    map<int, string, IntHash, IntEqual> map1;
    map1[1] = "One";
    map1[2] = "Two";
    map1[3] = "Three";

    // Create a map of strings to vectors of integers using a default hash function and equality comparison function
    map<string, vector<int>> map2;
    map2["A"] = {1, 3, 5, 7, 9};
    map2["B"] = {2, 4, 6, 8, 10};

    // Print the contents of the first map
    cout << "Contents of map1:" << endl;
    for (auto& [key, value] : map1) {
        cout << key << " -> " << value << endl;
    }

    // Print the contents of the second map
    cout << "Contents of map2:" << endl;
    for (auto& [key, value] : map2) {
        cout << key << " -> ";
        for (int num : value) {
            cout << num << " ";
        }
        cout << endl;
    }

    // Sort the vector of integers using a lambda function as the comparator
    sort(vec.begin(), vec.end(), [](int a, int b) { return a > b; });

    // Print the sorted vector
    cout << "Sorted vector:" << endl;
    for (int num : vec) {
        cout << num << " ";
    }
    cout << endl;

    // Create a vector of pairs of integers and strings
    vector<pair<int, string>> vec2 = {
        {1, "One"},
        {2, "Two"},
        {3, "Three"},
        {4, "Four"},
        {5, "Five"}
    };

    // Sort the vector of pairs by their second element using a custom comparator function
    sort(vec2.begin(), vec2.end(), SortBySecond());

    // Print the sorted vector of pairs
    cout << "Sorted vector of pairs:" << endl;
    for (auto& [key, value] : vec2) {
        cout << key << " -> " << value << endl;
    }

    return 0;
}
```

Explanation:

1. We define custom hash functions and equality comparison functions for integers to be used with the `map` container. This allows us to use integers as keys in the map and provides a way to determine equality between integers.

2. We create two maps: `map1` and `map2`. `map1` maps integers to strings, while `map2` maps strings to vectors of integers. We insert some key-value pairs into each map.

3. We print the contents of both maps to the console.

4. We sort the `vec` vector of integers using a lambda function as the comparator. The lambda function simply compares two integers and returns `true` if the first integer is greater than the second integer. This sorts the vector in descending order.

5. We print the sorted vector to the console.

6. We create a vector of pairs of integers and strings (`vec2`). Each pair consists of an integer and a string.

7. We sort the `vec2` vector of pairs by their second element (the string) using a custom comparator function (`SortBySecond`). This comparator function compares two pairs and returns `true` if the second element of the first pair is less than the second element of the second pair. This sorts the vector of pairs in ascending order by the string values.

8. We print the sorted vector of pairs to the console.

This code demonstrates the use of various C++ features, including maps, lambdas, custom hash functions and equality comparison functions, and sorting algorithms. It also showcases how to work with vectors and pairs of different data types.