```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <unordered_map>
#include <unordered_set>
#include <cmath>
#include <limits>
#include <string>

using namespace std;

// Define a custom comparator function for sorting pairs based on their second element.
bool compare_by_second(pair<int, int> a, pair<int, int> b)
{
    return a.second < b.second;
}

// Define a custom hash function for unordered_map/unordered_set that takes a pair as input.
struct pair_hash {
    size_t operator()(const pair<int, int>& p) const
    {
        return hash<int>()(p.first) ^ hash<int>()(p.second);
    }
};

int main()
{
    // Declare a vector of pairs.
    vector<pair<int, int>> pairs = {
        {1, 2},
        {3, 4},
        {5, 6},
        {7, 8}
    };

    // Sort the vector of pairs based on their second element using the custom comparator.
    sort(pairs.begin(), pairs.end(), compare_by_second);

    // Print the sorted vector of pairs.
    cout << "Sorted vector of pairs:" << endl;
    for (const auto& pair : pairs)
    {
        cout << "(" << pair.first << ", " << pair.second << ")" << endl;
    }

    // Declare an unordered_map that uses the custom hash function and maps integers to vectors of integers.
    unordered_map<int, vector<int>, pair_hash> unordered_map_int_to_vector_int;

    // Insert some data into the unordered_map.
    unordered_map_int_to_vector_int[1] = {2, 3, 4};
    unordered_map_int_to_vector_int[5] = {6, 7, 8};
    unordered_map_int_to_vector_int[9] = {10, 11, 12};

    // Print the contents of the unordered_map.
    cout << "Contents of the unordered_map:" << endl;
    for (const auto& pair : unordered_map_int_to_vector_int)
    {
        cout << pair.first << " -> ";
        for (const auto& value : pair.second)
        {
            cout << value << " ";
        }
        cout << endl;
    }

    // Declare a set of pairs.
    set<pair<int, int>> set_of_pairs;

    // Insert some data into the set of pairs.
    set_of_pairs.insert({1, 2});
    set_of_pairs.insert({3, 4});
    set_of_pairs.insert({5, 6});

    // Print the contents of the set of pairs.
    cout << "Contents of the set of pairs:" << endl;
    for (const auto& pair : set_of_pairs)
    {
        cout << "(" << pair.first << ", " << pair.second << ")" << endl;
    }

    // Declare an unordered_set of strings.
    unordered_set<string> unordered_set_of_strings;

    // Insert some data into the unordered_set of strings.
    unordered_set_of_strings.insert("Hello");
    unordered_set_of_strings.insert("World");
    unordered_set_of_strings.insert("!");

    // Print the contents of the unordered_set of strings.
    cout << "Contents of the unordered_set of strings:" << endl;
    for (const auto& string : unordered_set_of_strings)
    {
        cout << string << endl;
    }

    // Declare a map of integers to strings.
    map<int, string> map_int_to_string;

    // Insert some data into the map of integers to strings.
    map_int_to_string[1] = "One";
    map_int_to_string[2] = "Two";
    map_int_to_string[3] = "Three";

    // Print the contents of the map of integers to strings.
    cout << "Contents of the map of integers to strings:" << endl;
    for (const auto& pair : map_int_to_string)
    {
        cout << pair.first << " -> " << pair.second << endl;
    }

    return 0;
}
```

This code demonstrates the use of various standard C++ library data structures and algorithms, including vectors, pairs, sets, unordered maps, and maps. It also includes custom comparator functions and hash functions. The code is well-commented to explain its purpose and functionality.