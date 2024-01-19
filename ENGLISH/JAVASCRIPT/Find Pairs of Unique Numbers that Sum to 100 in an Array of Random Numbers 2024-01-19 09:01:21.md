```javascript
// Define a function to generate a random number between 0 and 99
function getRandomNumber() {
  return Math.floor(Math.random() * 100);
}

// Create an array of 100 random numbers
const numbers = [];
for (let i = 0; i < 100; i++) {
  numbers.push(getRandomNumber());
}

// Create a new array to store the unique numbers from the original array
const uniqueNumbers = [];

// Iterate over the original array and add each unique number to the new array
for (let i = 0; i < numbers.length; i++) {
  if (!uniqueNumbers.includes(numbers[i])) {
    uniqueNumbers.push(numbers[i]);
  }
}

// Sort the unique numbers in ascending order
uniqueNumbers.sort((a, b) => a - b);

// Create a new array to store the pairs of unique numbers that sum to 100
const pairs = [];

// Iterate over the unique numbers and check if any pairs sum to 100
for (let i = 0; i < uniqueNumbers.length; i++) {
  for (let j = i + 1; j < uniqueNumbers.length; j++) {
    if (uniqueNumbers[i] + uniqueNumbers[j] === 100) {
      pairs.push([uniqueNumbers[i], uniqueNumbers[j]]);
    }
  }
}

// Print the pairs of unique numbers that sum to 100
console.log(pairs);
```

This code generates an array of 100 random numbers, then finds all the unique numbers in the array and sorts them in ascending order. It then finds all the pairs of unique numbers that sum to 100 and prints them to the console.

The code uses a nested loop to check all the possible pairs of unique numbers. The outer loop iterates over the unique numbers from the beginning of the array, and the inner loop iterates over the unique numbers from the current index of the outer loop to the end of the array. This ensures that all possible pairs of unique numbers are checked.

The code uses the `includes()` method to check if a number is already in the `uniqueNumbers` array. This prevents duplicate numbers from being added to the array.

The code uses the `sort()` method to sort the `uniqueNumbers` array in ascending order. This makes it easier to find the pairs of numbers that sum to 100.

The code uses the `push()` method to add the pairs of numbers that sum to 100 to the `pairs` array.

Finally, the code uses the `console.log()` method to print the `pairs` array to the console.