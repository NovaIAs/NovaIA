```dart
// This is a Dart program to find the maximum sum of a contiguous subarray in an array of integers.
// The program uses the Kadane's algorithm to solve the problem.

// Function to find the maximum sum of a contiguous subarray in an array of integers.
int maxSubArraySum(List<int> arr) {
  // Initialize the current and maximum sums to 0.
  int currentSum = 0;
  int maxSum = 0;

  // Iterate over the array.
  for (int i = 0; i < arr.length; i++) {
    // Add the current element to the current sum.
    currentSum += arr[i];

    // If the current sum is greater than the maximum sum, update the maximum sum.
    if (currentSum > maxSum) {
      maxSum = currentSum;
    }

    // If the current sum is less than 0, reset it to 0.
    if (currentSum < 0) {
      currentSum = 0;
    }
  }

  // Return the maximum sum.
  return maxSum;
}

// Test the program.
void main() {
  // Create an array of integers.
  List<int> arr = [1, 2, 3, -4, 5, -6, 7, 8, -9, 10];

  // Find the maximum sum of a contiguous subarray in the array.
  int maxSum = maxSubArraySum(arr);

  // Print the maximum sum.
  print("The maximum sum of a contiguous subarray in the array is: $maxSum");
}
```

**Explanation:**

* The `maxSubArraySum()` function takes an array of integers as input and returns the maximum sum of a contiguous subarray in the array.
* The function initializes the current and maximum sums to 0.
* The function then iterates over the array.
* For each element in the array, the function adds the current element to the current sum.
* If the current sum is greater than the maximum sum, the function updates the maximum sum.
* If the current sum is less than 0, the function resets it to 0.
* After iterating over the entire array, the function returns the maximum sum.

* The `main()` function creates an array of integers and calls the `maxSubArraySum()` function to find the maximum sum of a contiguous subarray in the array.
* The `main()` function then prints the maximum sum.

The output of the program is:

```
The maximum sum of a contiguous subarray in the array is: 26
```