```
#include <iostream>
#include <vector>
#include <algorithm>
#include <map>
#include <set>

using namespace std;

// Define a custom comparator function for sorting pairs by their second value
bool compare_by_second(const pair<int, int>& a, const pair<int, int>& b) {
  return a.second < b.second;
}

int main() {
  // Create a vector of pairs of integers
  vector<pair<int, int>> vec = {
    {1, 3},
    {2, 5},
    {3, 1},
    {4, 2},
    {5, 4}
  };

  // Sort the vector using the custom comparator function
  sort(vec.begin(), vec.end(), compare_by_second);

  // Create a map to store the frequency of each integer in the vector
  map<int, int> freq;

  // Iterate over the vector and update the frequency of each integer
  for (const auto& pair : vec) {
    freq[pair.first]++;
  }

  // Create a set to store the distinct integers in the vector
  set<int> distinct;

  // Iterate over the vector and add each distinct integer to the set
  for (const auto& pair : vec) {
    distinct.insert(pair.first);
  }

  // Print the frequency of each integer
  cout << "Frequency of each integer:" << endl;
  for (const auto& pair : freq) {
    cout << pair.first << ": " << pair.second << endl;
  }

  // Print the distinct integers
  cout << "Distinct integers:" << endl;
  for (const auto& integer : distinct) {
    cout << integer << endl;
  }

  return 0;
}
```
Explanation:

1. We define a custom comparator function called `compare_by_second` that is used to sort pairs of integers by their second value.
2. We create a vector of pairs of integers and initialize it with some data.
3. We sort the vector using the `sort` function and the custom comparator function. This sorts the pairs in ascending order of their second value.
4. We create a map to store the frequency of each integer in the vector. We iterate over the vector and update the frequency of each integer in the map.
5. We create a set to store the distinct integers in the vector. We iterate over the vector and add each distinct integer to the set.
6. We print the frequency of each integer using the `cout` function.
7. We print the distinct integers using the `cout` function.