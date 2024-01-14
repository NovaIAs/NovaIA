```javascript
// Define a function to generate a random integer between min and max
function getRandomInt(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Define a function to generate a random array of length n with integers between min and max
function generateRandomArray(n, min, max) {
  const array = [];
  for (let i = 0; i < n; i++) {
    array.push(getRandomInt(min, max));
  }
  return array;
}

// Define a function to find the maximum value in an array
function findMax(array) {
  let maxValue = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > maxValue) {
      maxValue = array[i];
    }
  }
  return maxValue;
}

// Define a function to find the minimum value in an array
function findMin(array) {
  let minValue = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < minValue) {
      minValue = array[i];
    }
  }
  return minValue;
}

// Define a function to calculate the average value in an array
function findAverage(array) {
  let sum = 0;
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }
  return sum / array.length;
}

// Define a function to sort an array in ascending order
function sortAscending(array) {
  array.sort((a, b) => a - b);
  return array;
}

// Define a function to sort an array in descending order
function sortDescending(array) {
  array.sort((a, b) => b - a);
  return array;
}

// Generate a random array of 100 integers between 1 and 100
const array = generateRandomArray(100, 1, 100);

// Find the maximum value in the array
const maxValue = findMax(array);

// Find the minimum value in the array
const minValue = findMin(array);

// Calculate the average value in the array
const averageValue = findAverage(array);

// Sort the array in ascending order
const sortedAscending = sortAscending(array);

// Sort the array in descending order
const sortedDescending = sortDescending(array);

// Log the results to the console
console.log("Original array:", array);
console.log("Maximum value:", maxValue);
console.log("Minimum value:", minValue);
console.log("Average value:", averageValue);
console.log("Sorted ascending:", sortedAscending);
console.log("Sorted descending:", sortedDescending);
```

This code performs the following tasks:

1. It defines several helper functions:
   - `getRandomInt`: Generates a random integer between a specified minimum and maximum.
   - `generateRandomArray`: Generates a random array of a specified length with integers between a specified minimum and maximum.
   - `findMax`: Finds the maximum value in an array.
   - `findMin`: Finds the minimum value in an array.
   - `findAverage`: Calculates the average value in an array.
   - `sortAscending`: Sorts an array in ascending order.
   - `sortDescending`: Sorts an array in descending order.

2. It generates a random array of 100 integers between 1 and 100 using the `generateRandomArray` function.

3. It finds the maximum value in the array using the `findMax` function.

4. It finds the minimum value in the array using the `findMin` function.

5. It calculates the average value in the array using the `findAverage` function.

6. It sorts the array in ascending order using the `sortAscending` function.

7. It sorts the array in descending order using the `sortDescending` function.

8. Finally, it logs the original array, the maximum value, the minimum value, the average value, the sorted ascending array, and the sorted descending array to the console.