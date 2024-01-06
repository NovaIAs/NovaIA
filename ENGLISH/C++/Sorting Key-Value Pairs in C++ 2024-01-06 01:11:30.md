```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <utility>

using namespace std;

// Define a custom comparator function for sorting pairs by their second element (the value).
bool compareValues(const pair<int, int>& a, const pair<int, int>& b) {
    return a.second < b.second;
}

int main() {
    // Create a vector of pairs to store the key-value pairs.
    vector<pair<int, int>> keyValuePairs;

    // Add some key-value pairs to the vector.
    keyValuePairs.push_back(make_pair(1, 10));
    keyValuePairs.push_back(make_pair(2, 20));
    keyValuePairs.push_back(make_pair(3, 30));
    keyValuePairs.push_back(make_pair(4, 40));
    keyValuePairs.push_back(make_pair(5, 50));

    // Sort the vector of pairs by their second element (the value) using the custom comparator function.
    sort(keyValuePairs.begin(), keyValuePairs.end(), compareValues);

    // Create a map to store the sorted key-value pairs.
    map<int, int> sortedKeyValuePairs;

    // Insert the sorted key-value pairs into the map.
    for (const auto& pair : keyValuePairs) {
        sortedKeyValuePairs.insert(pair);
    }

    // Print the sorted key-value pairs.
    cout << "Sorted Key-Value Pairs:\n";
    for (const auto& pair : sortedKeyValuePairs) {
        cout << pair.first << " -> " << pair.second << endl;
    }

    return 0;
}
```

Explanation:

1. **Custom Comparator Function (`compareValues`):**
   - This function is used to sort pairs of integers based on their second element (the value).
   - It takes two pairs as input and returns `true` if the second element of the first pair is less than the second element of the second pair.
   - This function is passed as an argument to the `sort` function to sort the vector of pairs by their values.

2. **Sorting the Vector of Pairs (`keyValuePairs`):**
   - The `sort` function is used to sort the vector of pairs `keyValuePairs` using the `compareValues` comparator function.
   - After sorting, the vector contains the key-value pairs sorted in ascending order of their values.

3. **Creating a Map (`sortedKeyValuePairs`):**
   - A map is used to store the sorted key-value pairs.
   - Maps are ordered collections that maintain the keys in sorted order.

4. **Inserting the Sorted Key-Value Pairs into the Map:**
   - The sorted key-value pairs are inserted into the map using the `insert` function.
   - The `insert` function takes a pair as an argument and inserts it into the map.

5. **Printing the Sorted Key-Value Pairs:**
   - The sorted key-value pairs are printed using a range-based `for` loop.
   - The loop iterates over the map and prints each key-value pair.

This code demonstrates the use of a custom comparator function, sorting a vector of pairs, and inserting the sorted pairs into a map. It also shows how to print the sorted key-value pairs from the map.