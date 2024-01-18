```javascript
// Function to find the factorial of a number
function factorial(num) {
  // If the number is 0, return 1
  if (num === 0) {
    return 1;
  }

  // Recursive case: multiply the number by the factorial of the previous number
  return num * factorial(num - 1);
}

// Function to calculate the Fibonacci sequence using memoization
function fibonacci(n, memo = {}) {
  // If the number is 0 or 1, return the number itself
  if (n === 0 || n === 1) {
    return n;
  }

  // If the Fibonacci number for n has already been calculated, return it from the memo object
  if (memo[n]) {
    return memo[n];
  }

  // Recursive case: calculate the Fibonacci number for n and store it in the memo object
  const result = fibonacci(n - 1, memo) + fibonacci(n - 2, memo);
  memo[n] = result;

  // Return the result
  return result;
}

// Function to sort an array using the merge sort algorithm
function mergeSort(arr) {
  // If the array is empty or has only one element, it is already sorted
  if (arr.length <= 1) {
    return arr;
  }

  // Divide the array into two halves
  const mid = Math.floor(arr.length / 2);
  const leftHalf = arr.slice(0, mid);
  const rightHalf = arr.slice(mid);

  // Recursively sort the left and right halves
  const sortedLeftHalf = mergeSort(leftHalf);
  const sortedRightHalf = mergeSort(rightHalf);

  // Merge the two sorted halves into one sorted array
  return merge(sortedLeftHalf, sortedRightHalf);
}

// Helper function to merge two sorted arrays into one sorted array
function merge(left, right) {
  const mergedArray = [];
  let leftIndex = 0;
  let rightIndex = 0;

  // While both left and right arrays have elements, compare and add the smaller element to the merged array
  while (leftIndex < left.length && rightIndex < right.length) {
    if (left[leftIndex] < right[rightIndex]) {
      mergedArray.push(left[leftIndex]);
      leftIndex++;
    } else {
      mergedArray.push(right[rightIndex]);
      rightIndex++;
    }
  }

  // Add the remaining elements from either the left or right array to the merged array
  while (leftIndex < left.length) {
    mergedArray.push(left[leftIndex]);
    leftIndex++;
  }

  while (rightIndex < right.length) {
    mergedArray.push(right[rightIndex]);
    rightIndex++;
  }

  // Return the merged array
  return mergedArray;
}

// Function to find the maximum sum of a subarray in an array using Kadane's algorithm
function maxSubarraySum(arr) {
  // Initialize the current maximum sum and the overall maximum sum
  let currentMaxSum = 0;
  let overallMaxSum = Number.MIN_SAFE_INTEGER;

  // Iterate through the array
  for (let i = 0; i < arr.length; i++) {
    // Add the current element to the current maximum sum
    currentMaxSum += arr[i];

    // If the current maximum sum is greater than the overall maximum sum, update the overall maximum sum
    if (currentMaxSum > overallMaxSum) {
      overallMaxSum = currentMaxSum;
    }

    // If the current maximum sum is negative, reset it to 0
    if (currentMaxSum < 0) {
      currentMaxSum = 0;
    }
  }

  // Return the overall maximum sum
  return overallMaxSum;
}

// Function to find the longest common substring between two strings using dynamic programming
function longestCommonSubstring(str1, str2) {
  // Create a matrix to store the lengths of the longest common substrings of the two strings
  const matrix = new Array(str1.length + 1).fill(0).map(() => new Array(str2.length + 1).fill(0));

  // Iterate through the two strings and fill the matrix
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        matrix[i][j] = matrix[i - 1][j - 1] + 1;
      }
    }
  }

  // Find the maximum value in the matrix
  let maxLen = 0;
  let maxI = 0;
  let maxJ = 0;
  for (let i = 0; i <= str1.length; i++) {
    for (let j = 0; j <= str2.length; j++) {
      if (matrix[i][j] > maxLen) {
        maxLen = matrix[i][j];
        maxI = i;
        maxJ = j;
      }
    }
  }

  // Return the longest common substring
  return str1.substring(maxI - maxLen, maxI);
}

// Example usage
const result = fibonacci(10); // 55
const sortedArray = mergeSort([1, 5, 3, 2, 4]); // [1, 2, 3, 4, 5]
const maxSum = maxSubarraySum([-2, 1, -3, 4, -1, 2, 1, -5, 4]); // 6
const longestSubstring = longestCommonSubstring("ABCD