```dart
// A Dart function to find the equilibrium point of an array.
int equilibriumPoint(List<int> arr) {
  // Base case: If the array is empty, return -1.
  if (arr.isEmpty) {
    return -1;
  }

  // Calculate the sum of the array.
  int sum = arr.reduce((a, b) => a + b);

  // Iterate through the array and find the equilibrium point.
  int leftSum = 0;
  for (int i = 0; i < arr.length; i++) {
    leftSum += arr[i];

    // Check if the leftSum is equal to the rightSum.
    if (leftSum == sum - leftSum - arr[i]) {
      return i;
    }
  }

  // If no equilibrium point is found, return -1.
  return -1;
}

// A Dart function to find all the equilibrium points of an array.
List<int> equilibriumPoints(List<int> arr) {
  // Base case: If the array is empty, return an empty list.
  if (arr.isEmpty) {
    return [];
  }

  // Calculate the sum of the array.
  int sum = arr.reduce((a, b) => a + b);

  // Initialize the list of equilibrium points.
  List<int> equilibriumPoints = [];

  // Iterate through the array and find the equilibrium points.
  int leftSum = 0;
  for (int i = 0; i < arr.length; i++) {
    leftSum += arr[i];

    // Check if the leftSum is equal to the rightSum.
    if (leftSum == sum - leftSum - arr[i]) {
      equilibriumPoints.add(i);
    }
  }

  // Return the list of equilibrium points.
  return equilibriumPoints;
}

// A Dart function to find the equilibrium index of an array.
int equilibriumIndex(List<int> arr) {
  // Base case: If the array is empty, return -1.
  if (arr.isEmpty) {
    return -1;
  }

  // Calculate the sum of the array.
  int sum = arr.reduce((a, b) => a + b);

  // Iterate through the array and find the equilibrium index.
  int leftSum = 0;
  for (int i = 0; i < arr.length; i++) {
    leftSum += arr[i];

    // Check if the rightSum is equal to the leftSum.
    if (leftSum == sum - leftSum) {
      return i;
    }
  }

  // If no equilibrium index is found, return -1.
  return -1;
}

// A Dart function to find the equilibrium range of an array.
List<int> equilibriumRange(List<int> arr) {
  // Initialize the list of equilibrium ranges.
  List<int> equilibriumRanges = [];

  // Iterate through the array and find the equilibrium ranges.
  for (int i = 0; i < arr.length; i++) {
    int leftSum = 0;
    int rightSum = 0;

    // Calculate the left sum.
    for (int j = 0; j <= i; j++) {
      leftSum += arr[j];
    }

    // Calculate the right sum.
    for (int j = i + 1; j < arr.length; j++) {
      rightSum += arr[j];
    }

    // Check if the left and right sums are equal.
    if (leftSum == rightSum) {
      equilibriumRanges.add([i, i]);
    }
  }

  // Return the list of equilibrium ranges.
  return equilibriumRanges;
}

// A Dart function to find the equilibrium point of an array using prefix sums.
int equilibriumPointPrefixSum(List<int> arr) {
  // Base case: If the array is empty, return -1.
  if (arr.isEmpty) {
    return -1;
  }

  // Calculate the prefix sums of the array.
  List<int> prefixSums = [];
  prefixSums.add(arr[0]);
  for (int i = 1; i < arr.length; i++) {
    prefixSums.add(prefixSums.last + arr[i]);
  }

  // Find the equilibrium point.
  int equilibriumPoint = -1;
  for (int i = 1; i < prefixSums.length - 1; i++) {
    if (prefixSums[i] == prefixSums.last - prefixSums[i]) {
      equilibriumPoint = i;
      break;
    }
  }

  // Return the equilibrium point.
  return equilibriumPoint;
}

// A Dart function to find all the equilibrium points of an array using prefix sums.
List<int> equilibriumPointsPrefixSum(List<int> arr) {
  // Base case: If the array is empty, return an empty list.
  if (arr.isEmpty) {
    return [];
  }

  // Calculate the prefix sums of the array.
  List<int> prefixSums = [];
  prefixSums.add(arr[0]);
  for (int i = 1; i < arr.length; i++) {
    prefixSums.add(prefixSums.last + arr[i]);
  }

  // Initialize the list of equilibrium points.
  List<int> equilibriumPoints = [];

  // Find the equilibrium points.
  for (int i = 1; i < prefixSums.length - 1; i++) {
    if (prefixSums[i] == prefixSums.last - prefixSums[i]) {
      equilibriumPoints.add(i);
    }
  }

  // Return the list of equilibrium points.
  return equilibriumPoints;
}
```

This code contains six functions to find the equilibrium point or range of an array. An equilibrium point or range is a point or range in an array where the sum of elements on the left is equal to the sum of elements on the right.

The first function, `equilibriumPoint()`, finds the first equilibrium point in an array. It iterates through the array and checks if the sum of elements to the left is equal to the sum of elements to the right. If an equilibrium point is found, it returns the index of that point. Otherwise, it returns -1.

The second function, `equilibriumPoints()`, finds all the equilibrium points in an array. It works similarly to the first function, but it stores all the equilibrium points in a list and returns the list.

The third function, `equilibriumIndex()`, finds the equilibrium index of an array. The equilibrium index is the index where the sum of elements to the left is equal to the sum of elements to the right. If an equilibrium index is found, it returns the index. Otherwise, it returns -1.

The fourth function, `equilibriumRange()`, finds all the equilibrium ranges in an array. An equilibrium range is a range of indices where the sum of elements in the range is equal to the sum of elements outside the range. The function iterates through the array and checks if the sum of elements to the left and right of a given index are equal. If an equilibrium range is found, it stores the range in a list and returns the list.

The fifth function, `equilibriumPointPrefixSum()`, finds the equilibrium point of an array using prefix sums. Prefix sums are a data structure where the value at each index is the sum of all elements up to that index. Using prefix sums, the equilibrium point can be found in O(n) time, where n is the length of the array.

The sixth function, `equilibriumPointsPrefixSum()`, finds all the equilibrium points of an array using prefix sums. It works similarly to the fifth function, but it stores all the equilibrium points in a list and returns the list.