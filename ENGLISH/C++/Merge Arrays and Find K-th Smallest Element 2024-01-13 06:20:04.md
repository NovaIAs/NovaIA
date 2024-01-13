```c++
#include <iostream>
#include <vector>
#include <algorithm>
using namespace std;

// Define a custom comparator function for sorting vectors of pairs
bool comparePairs(pair<int, int> a, pair<int, int> b) {
  // Sort by the second element of each pair (the value)
  return a.second < b.second;
}

// Define a function to merge two sorted arrays
vector<int> mergeArrays(vector<int> arr1, vector<int> arr2) {
  // Create a new vector to store the merged array
  vector<int> mergedArray;

  // Use iterators to traverse both arrays
  vector<int>::iterator it1 = arr1.begin();
  vector<int>::iterator it2 = arr2.begin();

  // While both iterators are within their respective arrays
  while (it1 != arr1.end() && it2 != arr2.end()) {
    // If the current element in arr1 is less than or equal to the current element in arr2
    if (*it1 <= *it2) {
      // Add the current element from arr1 to the merged array
      mergedArray.push_back(*it1);

      // Advance the iterator for arr1
      it1++;
    }
    // Otherwise
    else {
      // Add the current element from arr2 to the merged array
      mergedArray.push_back(*it2);

      // Advance the iterator for arr2
      it2++;
    }
  }

  // Add the remaining elements from arr1, if any
  while (it1 != arr1.end()) {
    mergedArray.push_back(*it1);
    it1++;
  }

  // Add the remaining elements from arr2, if any
  while (it2 != arr2.end()) {
    mergedArray.push_back(*it2);
    it2++;
  }

  // Return the merged array
  return mergedArray;
}

// Define a function to find the k-th smallest element in an array
int findKthSmallest(vector<int> arr, int k) {
  // Sort the array in ascending order
  sort(arr.begin(), arr.end());

  // Return the k-th element of the sorted array
  return arr[k - 1];
}

// Define the main function
int main() {
  // Initialize two sorted vectors of integers
  vector<int> arr1 = {1, 3, 5, 7, 9};
  vector<int> arr2 = {2, 4, 6, 8, 10};

  // Merge the two vectors into a single sorted vector
  vector<int> mergedArray = mergeArrays(arr1, arr2);

  // Print the merged array
  cout << "Merged Array: ";
  for (int element : mergedArray) {
    cout << element << " ";
  }
  cout << endl;

  // Find the 3rd smallest element in the merged array
  int k = 3;
  int kthSmallest = findKthSmallest(mergedArray, k);

  // Print the k-th smallest element
  cout << "The " << k << "-th smallest element in the merged array is: " << kthSmallest << endl;

  return 0;
}
```

Explanation:

1. **Merge Arrays Function (mergeArrays):**

   - This function takes two sorted vectors of integers (`arr1` and `arr2`) and merges them into a single sorted vector.
   - It uses two iterators (`it1` and `it2`) to traverse both vectors and compares the current elements at each step.
   - The smaller element is added to the merged array, and the corresponding iterator is advanced.
   - The process continues until both iterators reach the end of their respective vectors.
   - Any remaining elements in either vector are appended to the merged array.

2. **Find K-th Smallest Function (findKthSmallest):**

   - This function takes a sorted vector of integers (`arr`) and an integer `k`, representing the k-th smallest element to be found.
   - It sorts the vector `arr` in ascending order using `std::sort`.
   - The k-th smallest element is then simply the element at index `k - 1` in the sorted vector.

3. **Main Function:**

   - In the main function, two sorted vectors `arr1` and `arr2` are initialized.
   - The `mergeArrays` function is called to merge these vectors into a single sorted vector called `mergedArray`.
   - The merged array is printed using a range-based for loop.
   - The `findKthSmallest` function is called to find the 3rd smallest element in the `mergedArray`.
   - The k-th smallest element is printed using `std::cout`.