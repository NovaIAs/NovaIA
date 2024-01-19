```javascript
// Create a function that takes an array of numbers and returns the sum of all the numbers in the array.
function sum(arr) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return 0;
  }

  // Initialize the sum to 0.
  let sum = 0;

  // Loop through the array and add each number to the sum.
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }

  // Return the sum.
  return sum;
}

// Create a function that takes a string and returns the number of vowels in the string.
function countVowels(str) {
  // Check if the string is empty.
  if (str.length === 0) {
    return 0;
  }

  // Initialize the vowel count to 0.
  let vowelCount = 0;

  // Loop through the string and check if each character is a vowel.
  for (let i = 0; i < str.length; i++) {
    const char = str[i].toLowerCase();
    if (char === 'a' || char === 'e' || char === 'i' || char === 'o' || char === 'u') {
      vowelCount++;
    }
  }

  // Return the vowel count.
  return vowelCount;
}

// Create a function that takes a number and returns the factorial of the number.
function factorial(num) {
  // Check if the number is less than 0.
  if (num < 0) {
    throw new Error("Factorial is not defined for negative numbers");
  }

  // Initialize the factorial to 1.
  let factorial = 1;

  // Loop through the numbers from 1 to the number and multiply them together.
  for (let i = 1; i <= num; i++) {
    factorial *= i;
  }

  // Return the factorial.
  return factorial;
}

// Create a function that takes a string and returns a reversed version of the string.
function reverseString(str) {
  // Check if the string is empty.
  if (str.length === 0) {
    return "";
  }

  // Create a new array to store the reversed string.
  const reversedStr = [];

  // Loop through the string from the end to the beginning and add each character to the reversed array.
  for (let i = str.length - 1; i >= 0; i--) {
    reversedStr.push(str[i]);
  }

  // Join the reversed array into a string and return it.
  return reversedStr.join("");
}

// Create a function that takes an array of objects and returns a new array of objects with the keys and values swapped.
function swapKeysAndValues(arr) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return [];
  }

  // Create a new array to store the swapped objects.
  const swappedArr = [];

  // Loop through the array of objects.
  for (let i = 0; i < arr.length; i++) {
    const object = arr[i];

    // Create a new object to store the swapped keys and values.
    const swappedObject = {};

    // Loop through the keys and values of the object.
    for (const key in object) {
      const value = object[key];

      // Add the value as the key and the key as the value in the swapped object.
      swappedObject[value] = key;
    }

    // Add the swapped object to the swapped array.
    swappedArr.push(swappedObject);
  }

  // Return the swapped array.
  return swappedArr;
}

// Create a function that takes an array of numbers and returns the maximum value in the array.
function findMax(arr) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return undefined;
  }

  // Initialize the maximum value to the first element in the array.
  let max = arr[0];

  // Loop through the array and update the maximum value if a larger value is found.
  for (let i = 1; i < arr.length; i++) {
    if (arr[i] > max) {
      max = arr[i];
    }
  }

  // Return the maximum value.
  return max;
}

// Create a function that takes an array of numbers and returns the minimum value in the array.
function findMin(arr) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return undefined;
  }

  // Initialize the minimum value to the first element in the array.
  let min = arr[0];

  // Loop through the array and update the minimum value if a smaller value is found.
  for (let i = 1; i < arr.length; i++) {
    if (arr[i] < min) {
      min = arr[i];
    }
  }

  // Return the minimum value.
  return min;
}

// Create a function that takes a string and returns a string with all the duplicate characters removed.
function removeDuplicateCharacters(str) {
  // Check if the string is empty.
  if (str.length === 0) {
    return "";
  }

  // Create a set to store the unique characters.
  const uniqueChars = new Set();

  // Loop through the string and add each character to the set.
  for (let i = 0; i < str.length; i++) {
    const char = str[i];
    uniqueChars.add(char);
  }

  // Join the unique characters into a string and return it.
  return [...uniqueChars].join("");
}

// Create a function that takes an array of numbers and returns a new array with the numbers sorted in ascending order.
function sortNumbers(arr) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return [];
  }

  // Sort the array in ascending order.
  arr.sort((a, b) => a - b);

  // Return the sorted array.
  return arr;
}

// Create a function that takes an array of objects and returns a new array with the objects sorted by a given key.
function sortByKey(arr, key) {
  // Check if the array is empty.
  if (arr.length === 0) {
    return [];
  }

  // Sort the array by the given key.
  arr.sort((a, b) => a[key] - b[key]);

  // Return the sorted array.
  return arr;
}

// Create a function that takes a string and returns a string with the first letter of each word capitalized.
function capitalizeFirstLetters(str) {
  // Check if the string is empty.
  if (str.length === 0) {
    return "";
  }

  // Split the string into an array of words.
  const words = str.split(" ");

  // Loop through the words and capitalize the first letter of each word.
  for (let i =