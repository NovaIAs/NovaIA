```c++
#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

// Define a custom comparator for sorting strings by their length
bool compareStringLength(const string& a, const string& b) {
    return a.length() < b.length();
}

// Define a custom comparator for sorting integers in descending order
bool compareIntegersDescending(const int& a, const int& b) {
    return a > b;
}

int main() {
    // Create a vector of strings
    vector<string> strings = {"Apple", "Banana", "Cherry", "Date", "Elderberry"};

    // Sort the strings in ascending order of their length using the custom comparator
    sort(strings.begin(), strings.end(), compareStringLength);

    // Print the sorted strings
    cout << "Strings sorted by length:" << endl;
    for (const string& s : strings) {
        cout << s << endl;
    }

    // Create a vector of integers
    vector<int> numbers = {1, 3, 5, 2, 4, 6};

    // Sort the integers in descending order using the custom comparator
    sort(numbers.begin(), numbers.end(), compareIntegersDescending);

    // Print the sorted integers
    cout << "Integers sorted in descending order:" << endl;
    for (const int& n : numbers) {
        cout << n << endl;
    }

    // Create a map from strings to integers
    map<string, int> fruits;
    fruits["Apple"] = 1;
    fruits["Banana"] = 2;
    fruits["Cherry"] = 3;
    fruits["Date"] = 4;
    fruits["Elderberry"] = 5;

    // Print the contents of the map
    cout << "Fruits and their corresponding numbers:" << endl;
    for (const auto& [fruit, number] : fruits) {
        cout << fruit << " -> " << number << endl;
    }

    // Create a set of strings
    set<string> uniqueFruits;
    uniqueFruits.insert("Apple");
    uniqueFruits.insert("Banana");
    uniqueFruits.insert("Cherry");
    uniqueFruits.insert("Date");
    uniqueFruits.insert("Elderberry");

    // Print the contents of the set
    cout << "Unique fruits:" << endl;
    for (const string& fruit : uniqueFruits) {
        cout << fruit << endl;
    }

    return 0;
}
```

This code demonstrates the use of various C++ standard library components, such as vectors, maps, and sets, to perform complex data manipulation and sorting tasks. It also shows how to define custom comparators for sorting data in specific orders.

Here's a breakdown of the code:

1. We define two custom comparators:
   - `compareStringLength` sorts strings by their length in ascending order.
   - `compareIntegersDescending` sorts integers in descending order.

2. We create a vector of strings, a vector of integers, a map from strings to integers, and a set of strings.

3. We use the `sort` function to sort the strings in the vector by their length using the `compareStringLength` comparator.

4. We sort the integers in the vector in descending order using the `compareIntegersDescending` comparator.

5. We add data to the map and print its contents.

6. We add data to the set and print its contents.

This code demonstrates the flexibility and power of C++ standard library components for working with complex data structures and performing various data manipulation tasks.