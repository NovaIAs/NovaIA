Certainly! Here's a complex and unique JavaScript code that implements a custom sorting algorithm called "Quicksort Plus". This algorithm is an enhanced version of the traditional Quicksort algorithm with additional optimizations for efficiency.

```javascript
/**
 * Quicksort Plus Algorithm
 * Sorts an array of numbers in ascending order.
 * This is an enhanced version of the Quicksort algorithm.
 */
function quicksortPlus(arr) {
  if (arr.length <= 1) {
    return arr;
  }

  const pivot = arr[Math.floor(Math.random() * arr.length)];
  const smaller = [];
  const equal = [];
  const larger = [];

  for (let num of arr) {
    if (num < pivot) {
      smaller.push(num);
    } else if (num === pivot) {
      equal.push(num);
    } else {
      larger.push(num);
    }
  }

  return quicksortPlus(smaller).concat(equal, quicksortPlus(larger));
}

// Test the algorithm
const unsortedArray = [9, 5, 7, 2, 4, 1, 8, 3, 6];
console.log("Original Array:", unsortedArray);

const sortedArray = quicksortPlus(unsortedArray);
console.log("Sorted Array:", sortedArray);
```

Explanation:
- The `quicksortPlus` function is a recursive function that takes an array `arr` as input and sorts it using the Quicksort Plus algorithm.
- The base case checks if the length of the array is less than or equal to 1. If true, it returns the array as it is already sorted.
- Otherwise, it selects a random pivot element from the array.
- It then initializes three empty arrays: `smaller`, `equal`, and `larger`.
- The function iterates through each element of the array and categorizes them into the appropriate sub-arrays based on their relation to the pivot element.
- Elements smaller than the pivot are pushed to the `smaller` array, elements equal to the pivot are pushed to the `equal` array, and elements larger than the pivot are pushed to the `larger` array.
- The function recursively calls `quicksortPlus` on the `smaller` and `larger` arrays, and concatenates the sorted sub-arrays with the `equal` array to form the final sorted array.
- Finally, the algorithm is tested with an example array `[9, 5, 7, 2, 4, 1, 8, 3, 6]`, and the original and sorted arrays are printed to the console.

Note: The Quicksort Plus algorithm takes advantage of the random pivot selection to improve performance on average cases by reducing the likelihood of worst-case scenarios.