```javascript
// Define a function to calculate the factorial of a number
const factorial = (n) => {
  if (n === 0) {
    return 1;
  }
  return n * factorial(n - 1);
};

// Define a function to generate a random string of a given length
const generateRandomString = (length) => {
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let result = '';
  for (let i = 0; i < length; i++) {
    result += characters.charAt(Math.floor(Math.random() * characters.length));
  }
  return result;
};

// Define a function to shuffle an array
const shuffleArray = (array) => {
  let currentIndex = array.length, randomIndex;

  // While there are elements in the array
  while (currentIndex != 0) {
    // Pick a random index
    randomIndex = Math.floor(Math.random() * currentIndex);
    currentIndex--;

    // Swap the elements at the current index and the random index
    [array[currentIndex], array[randomIndex]] = [array[randomIndex], array[currentIndex]];
  }

  return array;
};

// Define a function to merge two sorted arrays into one sorted array
const mergeSortedArrays = (array1, array2) => {
  let mergedArray = [];
  let i = 0, j = 0;

  // While both arrays have elements
  while (i < array1.length && j < array2.length) {
    // If the element in array1 is smaller, add it to the merged array
    if (array1[i] < array2[j]) {
      mergedArray.push(array1[i]);
      i++;
    } else {
      // Otherwise, add the element in array2 to the merged array
      mergedArray.push(array2[j]);
      j++;
    }
  }

  // Add the remaining elements from either array to the merged array
  while (i < array1.length) {
    mergedArray.push(array1[i]);
    i++;
  }
  while (j < array2.length) {
    mergedArray.push(array2[j]);
    j++;
  }

  return mergedArray;
};

// Define a function to find the longest common substring between two strings
const longestCommonSubstring = (string1, string2) => {
  let longestSubstring = '';
  let currentSubstring = '';

  // Iterate over the first string
  for (let i = 0; i < string1.length; i++) {
    // Iterate over the second string starting from the current index in the first string
    for (let j = i; j < string2.length; j++) {
      // If the current characters in both strings match, extend the current substring
      if (string1[i] === string2[j]) {
        currentSubstring += string1[i];
      } else {
        // Otherwise, reset the current substring and move on to the next character in the second string
        currentSubstring = '';
      }

      // If the current substring is longer than the longest substring so far, update the longest substring
      if (currentSubstring.length > longestSubstring.length) {
        longestSubstring = currentSubstring;
      }
    }
  }

  return longestSubstring;
};

// Define a function to convert a number to Roman numerals
const numberToRoman = (number) => {
  const romanNumerals = {
    1: 'I',
    4: 'IV',
    5: 'V',
    9: 'IX',
    10: 'X',
    40: 'XL',
    50: 'L',
    90: 'XC',
    100: 'C',
    400: 'CD',
    500: 'D',
    900: 'CM',
    1000: 'M'
  };

  let romanNumeral = '';
  for (let i = 1000; i >= 1; i /= 10) {
    while (number >= i) {
      romanNumeral += romanNumerals[i];
      number -= i;
    }
  }

  return romanNumeral;
};

// Define a function to find the missing number in an array of consecutive numbers
const findMissingNumber = (array) => {
  const expectedSum = (array.length + 1) * (array.length + 2) / 2;
  const actualSum = array.reduce((a, b) => a + b, 0);
  return expectedSum - actualSum;
};

// Define a function to find the maximum subarray sum in an array of numbers
const maxSubarraySum = (array) => {
  let currentSum = 0;
  let maxSum = -Infinity;

  for (let i = 0; i < array.length; i++) {
    currentSum += array[i];
    if (currentSum > maxSum) {
      maxSum = currentSum;
    }
    if (currentSum < 0) {
      currentSum = 0;