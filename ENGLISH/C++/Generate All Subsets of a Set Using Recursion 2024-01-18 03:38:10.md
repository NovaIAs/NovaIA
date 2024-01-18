```c++
#include <bits/stdc++.h>

using namespace std;

// Define a macro for convenience.
#define FOR(i, a, b) for (int i = (a); i < (b); ++i)

// This function generates all possible subsets of a given set.
// It takes a vector of integers as input and returns a vector of vectors of integers,
// where each vector in the output represents a subset of the input set.
vector<vector<int>> subsets(vector<int> set) {
  // Check if the input set is empty.
  if (set.empty()) {
    return {{}}; // Return an empty vector, which represents the empty set.
  }

  // Remove the first element from the set.
  int first = set[0];
  set.erase(set.begin());

  // Generate all possible subsets of the remaining set.
  vector<vector<int>> subsets_remaining = subsets(set);

  // Create a new vector to store all possible subsets of the original set.
  vector<vector<int>> subsets_all;

  // For each subset of the remaining set, add the first element back to it.
  FOR(i, 0, subsets_remaining.size()) {
    subsets_remaining[i].push_back(first);
  }

  // Combine the subsets of the remaining set with the subsets that include the first element.
  subsets_all.insert(subsets_all.end(), subsets_remaining.begin(), subsets_remaining.end());
  subsets_all.insert(subsets_all.end(), subsets_remaining.begin(), subsets_remaining.end());

  // Return the vector of all possible subsets of the original set.
  return subsets_all;
}

// This function prints a vector of vectors of integers to the console.
// It is used to print the output of the subsets function.
void print_subsets(vector<vector<int>> subsets) {
  FOR(i, 0, subsets.size()) {
    cout << "[";
    FOR(j, 0, subsets[i].size()) {
      cout << subsets[i][j];
      if (j < subsets[i].size() - 1) {
        cout << ", ";
      }
    }
    cout << "]" << endl;
  }
}

int main() {
  // Create a vector of integers to generate subsets for.
  vector<int> set = {1, 2, 3};

  // Generate all possible subsets of the set.
  vector<vector<int>> subsets_all = subsets(set);

  // Print the subsets to the console.
  print_subsets(subsets_all);

  return 0;
}
```

Explanation:

* The `subsets` function takes a vector of integers as input and returns a vector of vectors of integers, where each vector in the output represents a subset of the input set.
* The function first checks if the input set is empty. If it is, it returns an empty vector, which represents the empty set.
* If the input set is not empty, the function removes the first element from the set and generates all possible subsets of the remaining set using recursion.
* The function then creates a new vector to store all possible subsets of the original set.
* For each subset of the remaining set, the function adds the first element back to it.
* The function then combines the subsets of the remaining set with the subsets that include the first element.
* Finally, the function returns the vector of all possible subsets of the original set.
* The `print_subsets` function is used to print a vector of vectors of integers to the console. It is used to print the output of the `subsets` function.
* The `main` function creates a vector of integers to generate subsets for, calls the `subsets` function to generate all possible subsets of the set, and then calls the `print_subsets` function to print the subsets to the console.