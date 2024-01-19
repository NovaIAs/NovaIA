```c++
#include <iostream>
#include <vector>
#include <unordered_map>
#include <algorithm>

using namespace std;

// Define a custom comparator function for sorting pairs by their second element (the value).
struct CompareByValue {
    bool operator()(const pair<int, int>& p1, const pair<int, int>& p2) {
        return p1.second > p2.second;
    }
};

// Function to find the top K most frequent elements in an array.
vector<int> findTopKFrequentElements(vector<int>& nums, int k) {
    // Create an unordered map to store the frequency of each element.
    unordered_map<int, int> frequencyMap;
    for (int num : nums) {
        frequencyMap[num]++;
    }

    // Convert the frequency map to a vector of pairs, where each pair consists of an element and its frequency.
    vector<pair<int, int>> frequencyVector(frequencyMap.begin(), frequencyMap.end());

    // Sort the vector of pairs by the frequency of the elements in descending order.
    sort(frequencyVector.begin(), frequencyVector.end(), CompareByValue());

    // Extract the top K elements from the sorted vector.
    vector<int> topKFrequentElements;
    for (int i = 0; i < k; i++) {
        topKFrequentElements.push_back(frequencyVector[i].first);
    }

    return topKFrequentElements;
}

int main() {
    // Example usage:
    vector<int> nums = {1, 1, 1, 2, 2, 3, 3, 3, 4, 4, 4, 4, 5, 5, 5, 6, 6, 6, 6};
    int k = 2;

    vector<int> topKFrequentElements = findTopKFrequentElements(nums, k);

    cout << "Top " << k << " most frequent elements in the array:" << endl;
    for (int num : topKFrequentElements) {
        cout << num << " ";
    }

    cout << endl;

    return 0;
}
```

Explanation:

1. We define a `CompareByValue` struct that implements a custom comparator function for sorting pairs by their second element (the value).
2. The `findTopKFrequentElements` function takes a vector of integers `nums` and an integer `k` as input and returns a vector of integers containing the top K most frequent elements in `nums`.
3. We create an unordered map `frequencyMap` to store the frequency of each element in `nums`.
4. We convert the unordered map to a vector of pairs `frequencyVector`, where each pair consists of an element and its frequency.
5. We sort the vector of pairs `frequencyVector` by the frequency of the elements in descending order using the `CompareByValue` comparator.
6. We extract the top K elements from the sorted vector `frequencyVector` and store them in a vector `topKFrequentElements`.
7. We return the vector `topKFrequentElements`.

In the `main` function:

1. We create a sample vector `nums` and an integer `k`.
2. We call the `findTopKFrequentElements` function to find the top K most frequent elements in `nums`.
3. We print the top K most frequent elements to the console.