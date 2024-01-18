```javascript
// This function takes a string as input and returns a new string with the characters in reverse order.
const reverseString = (string) => {
  // Check if the input is a valid string.
  if (typeof string !== "string") {
    throw new Error("Input must be a string.");
  }

  // Create an empty string to store the reversed string.
  let reversedString = "";

  // Iterate over the input string from the end to the beginning.
  for (let i = string.length - 1; i >= 0; i--) {
    // Append the current character to the reversed string.
    reversedString += string[i];
  }

  // Return the reversed string.
  return reversedString;
};

// This function takes an array of numbers as input and returns the sum of all the numbers in the array.
const sumArray = (array) => {
  // Check if the input is a valid array.
  if (!Array.isArray(array)) {
    throw new Error("Input must be an array.");
  }

  // Check if all the elements in the array are numbers.
  for (let i = 0; i < array.length; i++) {
    if (typeof array[i] !== "number") {
      throw new Error("All elements in the array must be numbers.");
    }
  }

  // Initialize the sum to 0.
  let sum = 0;

  // Iterate over the array and add each element to the sum.
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }

  // Return the sum.
  return sum;
};

// This function takes an object as input and returns a new object with the keys and values swapped.
const swapKeysAndValues = (object) => {
  // Check if the input is a valid object.
  if (typeof object !== "object") {
    throw new Error("Input must be an object.");
  }

  // Create an empty object to store the swapped object.
  let swappedObject = {};

  // Iterate over the keys in the input object.
  for (let key in object) {
    // Get the value associated with the current key.
    let value = object[key];

    // Add the value as a key to the swapped object.
    swappedObject[value] = key;
  }

  // Return the swapped object.
  return swappedObject;
};

// This function takes a function as input and returns a new function that will only execute the input function after a specified delay.
const debounce = (func, delay) => {
  // Check if the input is a valid function.
  if (typeof func !== "function") {
    throw new Error("Input must be a function.");
  }

  // Check if the delay is a valid number.
  if (typeof delay !== "number") {
    throw new Error("Delay must be a number.");
  }

  // Create a timeout variable to store the timeout ID.
  let timeout;

  // Return a new function that will execute the input function after the specified delay.
  return (...args) => {
    // Clear the previous timeout if it exists.
    clearTimeout(timeout);

    // Set a new timeout to execute the input function after the specified delay.
    timeout = setTimeout(() => {
      // Execute the input function with the arguments passed to the new function.
      func(...args);
    }, delay);
  };
};

// This function takes a string as input and returns a new string with all the duplicate characters removed.
const removeDuplicates = (string) => {
  // Check if the input is a valid string.
  if (typeof string !== "string") {
    throw new Error("Input must be a string.");
  }

  // Create a set to store the unique characters in the string.
  let uniqueCharacters = new Set();

  // Create an empty string to store the string with duplicate characters removed.
  let uniqueString = "";

  // Iterate over the string and add each character to the set.
  for (let i = 0; i < string.length; i++) {
    uniqueCharacters.add(string[i]);
  }

  // Iterate over the set and add each character to the unique string.
  for (let character of uniqueCharacters) {
    uniqueString += character;
  }

  // Return the unique string.
  return uniqueString;
};

// This function takes an array as input and returns a new array with all the elements sorted in ascending order.
const sortArray = (array) => {
  // Check if the input is a valid array.
  if (!Array.isArray(array)) {
    throw new Error("Input must be an array.");
  }

  // Sort the array in ascending order using the built-in sort() method.
  array.sort((a, b) => a - b);

  // Return the sorted array.
  return array;
};

// This function takes two arrays as input and returns a new array with the elements from both arrays combined.
const concatArrays = (array1, array2) => {
  // Check if the input is two valid arrays.
  if (!Array.isArray(array1) || !Array.isArray(array2)) {
    throw new Error("Input must be two arrays.");
  }

  // Combine the two arrays using the built-in concat() method.
  let combinedArray = array1.concat(array2);

  // Return the combined array.
  return combinedArray;
};

// This function takes a string as input and returns a new string with all the vowels removed.
const removeVowels = (string) => {
  // Check if the input is a valid string.
  if (typeof string !== "string") {
    throw new Error("Input must be a string.");
  }

  // Create a regular expression to match vowels.
  let vowelRegex = /[aeiouAEIOU]/g;

  // Use the regular expression to replace all the vowels with an empty string.
  let vowel