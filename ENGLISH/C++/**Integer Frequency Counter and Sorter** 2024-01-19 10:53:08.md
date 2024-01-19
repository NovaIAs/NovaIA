```c++
#include <iostream>
#include <vector>
#include <map>
#include <algorithm>
#include <functional>

using namespace std;

// Define a custom comparator function for sorting pairs based on their second element
struct compareSecond {
    bool operator()(const pair<int, int>& a, const pair<int, int>& b) {
        return a.second < b.second;
    }
};

// Main function
int main() {
    // Create a vector of integers
    vector<int> numbers = {1, 3, 5, 2, 4, 6};

    // Create a map to store the frequency of each integer
    map<int, int> frequencyMap;

    // Insert each integer into the map and increment its frequency
    for (int num : numbers) {
        frequencyMap[num]++;
    }

    // Create a vector of pairs to store the integer-frequency pairs
    vector<pair<int, int>> frequencyPairs;

    // Copy the integer-frequency pairs from the map to the vector
    copy(frequencyMap.begin(), frequencyMap.end(), back_inserter(frequencyPairs));

    // Sort the vector of pairs based on the frequency in descending order
    sort(frequencyPairs.begin(), frequencyPairs.end(), compareSecond());

    // Print the integer-frequency pairs in sorted order
    for (const auto& pair : frequencyPairs) {
        cout << pair.first << " appears " << pair.second << " times" << endl;
    }

    return 0;
}
```

This code demonstrates how to find and print the frequency of each integer in a given vector of integers in sorted order. Here's a step-by-step explanation of the code:

1. **Create a Vector of Integers**:
   - Declare a vector named `numbers` to store a list of integers.
   - Initialize the vector with some sample integers, such as `1, 3, 5, 2, 4, 6`.

2. **Create a Map to Store Frequencies**:
   - Declare a map named `frequencyMap` to store the frequency of each integer.
   - The map uses integers as keys (the unique integers) and integers as values (the frequencies).

3. **Insert Integers and Increment Frequencies**:
   - Iterate through the `numbers` vector using a range-based `for` loop.
   - For each integer `num`, insert it into the `frequencyMap` map.
   - If the integer is already present in the map, increment its frequency by 1.

4. **Create a Vector of Integer-Frequency Pairs**:
   - Declare a vector named `frequencyPairs` to store pairs of integers.
   - Each pair consists of an integer (the key from the `frequencyMap`) and its frequency (the value from the `frequencyMap`).

5. **Copy Integer-Frequency Pairs from Map to Vector**:
   - Use the `copy` function to copy the integer-frequency pairs from the `frequencyMap` to the `frequencyPairs` vector.

6. **Sort the Vector of Pairs by Frequency**:
   - Declare a custom comparator function `compareSecond` that compares the second element (frequency) of each pair.
   - Use the `sort` function to sort the `frequencyPairs` vector in descending order based on the frequency using the `compareSecond` comparator.

7. **Print the Sorted Integer-Frequency Pairs**:
   - Iterate through the sorted `frequencyPairs` vector using a range-based `for` loop.
   - For each pair, print the integer and its frequency in the format: "Integer appears Frequency times".

This code effectively counts the frequency of each integer in the given vector and displays the integer-frequency pairs in sorted order, with the most frequent integers appearing first.