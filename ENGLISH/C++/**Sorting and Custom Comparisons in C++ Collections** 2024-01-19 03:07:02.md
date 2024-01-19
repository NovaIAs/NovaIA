```c++
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>
#include <queue>
#include <stack>
#include <string>

using namespace std;

// Define a custom comparator for sorting pairs based on the second element
struct CompareSecond {
    bool operator()(const pair<int, int>& p1, const pair<int, int>& p2) {
        return p1.second < p2.second;
    }
};

// Define a custom comparator for sorting strings based on their length
struct CompareStringLength {
    bool operator()(const string& s1, const string& s2) {
        return s1.length() < s2.length();
    }
};

int main() {
    // Create a vector of integers and initialize it with some values
    vector<int> vecInt { 1, 3, 5, 2, 4, 6 };

    // Print the original vector
    cout << "Original vector: ";
    for (int i : vecInt) {
        cout << i << " ";
    }
    cout << endl;

    // Sort the vector in ascending order
    sort(vecInt.begin(), vecInt.end());

    // Print the sorted vector
    cout << "Sorted vector: ";
    for (int i : vecInt) {
        cout << i << " ";
    }
    cout << endl << endl;

    // Create a map of strings to integers and initialize it with some key-value pairs
    map<string, int> mapStrInt { { "one", 1 }, { "two", 2 }, { "three", 3 }, { "four", 4 } };

    // Print the original map
    cout << "Original map:" << endl;
    for (auto& [key, value] : mapStrInt) {
        cout << key << ": " << value << endl;
    }
    cout << endl;

    // Sort the map by the values using a custom comparator
    sort(mapStrInt.begin(), mapStrInt.end(), CompareSecond());

    // Print the sorted map
    cout << "Sorted map by values:" << endl;
    for (auto& [key, value] : mapStrInt) {
        cout << key << ": " << value << endl;
    }
    cout << endl;

    // Create a set of strings and initialize it with some values
    set<string> setStr { "apple", "banana", "cherry", "durian", "elderberry", "fig" };

    // Print the original set
    cout << "Original set: ";
    for (string s : setStr) {
        cout << s << " ";
    }
    cout << endl;

    // Sort the set in ascending order
    set<string> sortedSet(setStr.begin(), setStr.end());

    // Print the sorted set
    cout << "Sorted set: ";
    for (string s : sortedSet) {
        cout << s << " ";
    }
    cout << endl << endl;

    // Create a priority queue of integers and initialize it with some values
    priority_queue<int> pqInt;
    pqInt.push(3);
    pqInt.push(1);
    pqInt.push(5);
    pqInt.push(2);
    pqInt.push(4);

    // Print the original priority queue
    cout << "Original priority queue: ";
    while (!pqInt.empty()) {
        cout << pqInt.top() << " ";
        pqInt.pop();
    }
    cout << endl;

    // Sort the priority queue in ascending order using a custom comparator
    priority_queue<int, vector<int>, greater<int>> sortedPqInt(pqInt);

    // Print the sorted priority queue
    cout << "Sorted priority queue: ";
    while (!sortedPqInt.empty()) {
        cout << sortedPqInt.top() << " ";
        sortedPqInt.pop();
    }
    cout << endl << endl;

    // Create a stack of strings and initialize it with some values
    stack<string> stackStr;
    stackStr.push("apple");
    stackStr.push("banana");
    stackStr.push("cherry");
    stackStr.push("durian");
    stackStr.push("elderberry");

    // Print the original stack
    cout << "Original stack: ";
    while (!stackStr.empty()) {
        cout << stackStr.top() << " ";
        stackStr.pop();
    }
    cout << endl;

    // Sort the stack in ascending order using a custom comparator
    stack<string> sortedStackStr;
    while (!stackStr.empty()) {
        string top = stackStr.top();
        stackStr.pop();
        while (!sortedStackStr.empty() && sortedStackStr.top() > top) {
            stackStr.push(sortedStackStr.top());
            sortedStackStr.pop();
        }
        sortedStackStr.push(top);
    }

    // Print the sorted stack
    cout << "Sorted stack: ";
    while (!sortedStackStr.empty()) {
        cout << sortedStackStr.top() << " ";
        sortedStackStr.pop();
    }
    cout << endl;

    return 0;
}
```

Explanation:

1. We start by including necessary C++ libraries.

2. We define two custom comparators:

   - `CompareSecond` compares pairs based on their second elements.
   - `CompareStringLength` compares strings based on their lengths.

3. In the `main` function, we:

   - Create a vector of integers and initialize it with values.
   - Create a map of strings to integers and initialize it with key-value pairs.
   - Create a set of strings and initialize it with values.
   - Create a priority queue of integers and initialize it with values.
   - Create a stack of strings and initialize it with values.

4. We print the original versions of these data structures.

5. We sort each data structure:

   - For the vector, we use `sort` function.
   - For the map, we use `sort` function with a custom comparator.
   - For the set, we create a new set by sorting the elements of the original set.
   - For the priority queue, we create a new priority queue by sorting the elements of the original priority queue using a custom comparator.
   - For the stack, we use a custom algorithm to sort the elements while maintaining the LIFO order.

6. We print the sorted versions of the data structures.