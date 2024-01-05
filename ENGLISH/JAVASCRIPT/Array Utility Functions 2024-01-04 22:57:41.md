```javascript
// Define a function that takes an array of numbers and returns the sum of all the numbers in the array.
const sumArray = (numbers) => {
  // Initialize a variable to store the sum of the numbers.
  let sum = 0;

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // Add the current number to the sum.
    sum += numbers[i];
  }

  // Return the sum of the numbers.
  return sum;
};

// Define a function that takes an array of numbers and returns the average of all the numbers in the array.
const averageArray = (numbers) => {
  // Calculate the sum of the numbers in the array.
  const sum = sumArray(numbers);

  // Calculate the average of the numbers in the array.
  const average = sum / numbers.length;

  // Return the average of the numbers.
  return average;
};

// Define a function that takes an array of numbers and returns the maximum value in the array.
const maxArray = (numbers) => {
  // Set the initial maximum value to the first number in the array.
  let max = numbers[0];

  // Iterate over the array of numbers.
  for (let i = 1; i < numbers.length; i++) {
    // If the current number is greater than the current maximum value, update the maximum value to the current number.
    if (numbers[i] > max) {
      max = numbers[i];
    }
  }

  // Return the maximum value in the array.
  return max;
};

// Define a function that takes an array of numbers and returns the minimum value in the array.
const minArray = (numbers) => {
  // Set the initial minimum value to the first number in the array.
  let min = numbers[0];

  // Iterate over the array of numbers.
  for (let i = 1; i < numbers.length; i++) {
    // If the current number is less than the current minimum value, update the minimum value to the current number.
    if (numbers[i] < min) {
      min = numbers[i];
    }
  }

  // Return the minimum value in the array.
  return min;
};

// Define a function that takes an array of numbers and returns a new array with all the duplicate numbers removed.
const removeDuplicates = (numbers) => {
  // Create a new set to store the unique numbers.
  const uniqueNumbers = new Set();

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // Add the current number to the set.
    uniqueNumbers.add(numbers[i]);
  }

  // Convert the set of unique numbers back to an array.
  const uniqueNumbersArray = Array.from(uniqueNumbers);

  // Return the array of unique numbers.
  return uniqueNumbersArray;
};

// Define a function that takes an array of numbers and returns a new array with all the numbers sorted in ascending order.
const sortArray = (numbers) => {
  // Sort the array of numbers in ascending order.
  numbers.sort((a, b) => a - b);

  // Return the sorted array of numbers.
  return numbers;
};

// Define a function that takes an array of numbers and returns a new array with all the numbers sorted in descending order.
const sortArrayDescending = (numbers) => {
  // Sort the array of numbers in descending order.
  numbers.sort((a, b) => b - a);

  // Return the sorted array of numbers.
  return numbers;
};

// Define a function that takes an array of numbers and a number and returns a new array with all the numbers that are greater than the given number.
const filterGreaterThan = (numbers, number) => {
  // Create a new array to store the numbers that are greater than the given number.
  const greaterThanNumbers = [];

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // If the current number is greater than the given number, add it to the new array.
    if (numbers[i] > number) {
      greaterThanNumbers.push(numbers[i]);
    }
  }

  // Return the new array with all the numbers that are greater than the given number.
  return greaterThanNumbers;
};

// Define a function that takes an array of numbers and a number and returns a new array with all the numbers that are less than the given number.
const filterLessThan = (numbers, number) => {
  // Create a new array to store the numbers that are less than the given number.
  const lessThanNumbers = [];

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // If the current number is less than the given number, add it to the new array.
    if (numbers[i] < number) {
      lessThanNumbers.push(numbers[i]);
    }
  }

  // Return the new array with all the numbers that are less than the given number.
  return lessThanNumbers;
};

// Define a function that takes an array of numbers and a number and returns a new array with all the numbers that are equal to the given number.
const filterEqual = (numbers, number) => {
  // Create a new array to store the numbers that are equal to the given number.
  const equalNumbers = [];

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // If the current number is equal to the given number, add it to the new array.
    if (numbers[i] === number) {
      equalNumbers.push(numbers[i]);
    }
  }

  // Return the new array with all the numbers that are equal to the given number.
  return equalNumbers;
};

// Define a function that takes an array of numbers and returns a new array with all the numbers that are odd.
const filterOdd = (numbers) => {
  // Create a new array to store the odd numbers.
  const oddNumbers = [];

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // If the current number is odd, add it to the new array.
    if (numbers[i] % 2 !== 0) {
      oddNumbers.push(numbers[i]);
    }
  }

  // Return the new array with all the odd numbers.
  return oddNumbers;
};

// Define a function that takes an array of numbers and returns a new array with all the numbers that are even.
const filterEven = (numbers) => {
  // Create a new array to store the even numbers.
  const evenNumbers = [];

  // Iterate over the array of numbers.
  for (let i = 0; i < numbers.length; i++) {
    // If the current number is even, add it to the new array.
    if (numbers[i] % 2 === 0) {
      evenNumbers.push(numbers[i]);
    }
  }

  // Return the new array with all the even numbers.
  return evenNumbers;
};
```

This code is a collection of utility functions that can be used to perform various operations on arrays of numbers. The functions include:

- `sumArray`: Calculates the sum of all the numbers in an array.
- `averageArray`: Calculates the average of all the numbers in an array.
- `maxArray`: Finds the maximum value in an array.
- `minArray`: Finds the minimum value in an array.
- `removeDuplicates`: Removes all the duplicate numbers from an array.
- `sortArray`: Sorts an array of numbers in ascending order.
- `sortArrayDescending`: Sorts an array of numbers in descending order.
- `filterGreaterThan`: Filters an array of numbers to include only the numbers that are greater than a given number.
- `filterLessThan`: Filters an array of numbers to include only the numbers that are less than a given number.
- `filterEqual`: Filters an array of numbers to include only the numbers that are equal to a given number.
- `filterOdd`: Filters an array of numbers to include only the odd numbers.
- `filterEven`: Filters an array of numbers to include only the even numbers.

These functions can be used to perform a variety of tasks, such as calculating statistics, removing duplicate values, sorting arrays, and filtering arrays based on various criteria.