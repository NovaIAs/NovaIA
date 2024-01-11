```typescript
// This is a function that takes in a string and returns a boolean value indicating whether or not the string is a palindrome.
function isPalindrome(str) {
  // Convert the string to lowercase and remove all spaces.
  const cleanStr = str.toLowerCase().replace(/\s/g, '');

  // Reverse the string.
  const reversedStr = cleanStr.split('').reverse().join('');

  // Compare the original string to the reversed string.
  return cleanStr === reversedStr;
}

// This is a function that takes in an array of numbers and returns the sum of all the numbers in the array.
function sumArray(arr) {
  // Initialize a variable to store the sum.
  let sum = 0;

  // Iterate over the array and add each number to the sum.
  for (let i = 0; i < arr.length; i++) {
    sum += arr[i];
  }

  // Return the sum.
  return sum;
}

// This is a function that takes in an array of numbers and returns the average of all the numbers in the array.
function averageArray(arr) {
  // Get the sum of all the numbers in the array.
  const sum = sumArray(arr);

  // Divide the sum by the length of the array to get the average.
  const average = sum / arr.length;

  // Return the average.
  return average;
}

// This is a function that takes in an array of numbers and returns the median of the array.
function medianArray(arr) {
  // Sort the array in ascending order.
  arr.sort((a, b) => a - b);

  // If the array has an even number of elements, the median is the average of the two middle elements.
  if (arr.length % 2 === 0) {
    const mid1 = arr[Math.floor(arr.length / 2) - 1];
    const mid2 = arr[Math.floor(arr.length / 2)];
    return (mid1 + mid2) / 2;
  }

  // If the array has an odd number of elements, the median is the middle element.
  else {
    return arr[Math.floor(arr.length / 2)];
  }
}

// This is a function that takes in an array of numbers and returns the mode of the array.
function modeArray(arr) {
  // Create an object to store the frequency of each number in the array.
  const frequency = {};

  // Iterate over the array and count the frequency of each number.
  for (let i = 0; i < arr.length; i++) {
    const num = arr[i];
    if (frequency[num]) {
      frequency[num]++;
    } else {
      frequency[num] = 1;
    }
  }

  // Find the number with the highest frequency.
  let maxFrequency = 0;
  let mode;
  for (const num in frequency) {
    if (frequency[num] > maxFrequency) {
      maxFrequency = frequency[num];
      mode = num;
    }
  }

  // Return the mode.
  return mode;
}

// This is a function that takes in a string and returns the longest substring without repeating characters.
function longestSubstringWithoutRepeatingCharacters(str) {
  // Initialize a set to store the characters in the current substring.
  const charSet = new Set();

  // Initialize two pointers to mark the start and end of the current substring.
  let left = 0;
  let right = 0;

  // Initialize a variable to store the length of the longest substring.
  let maxLength = 0;

  // Iterate over the string.
  while (right < str.length) {
    // If the current character is not in the set, add it to the set and move the right pointer forward.
    if (!charSet.has(str[right])) {
      charSet.add(str[right]);
      right++;

      // If the length of the current substring is greater than the length of the longest substring, update the longest substring.
      if (right - left > maxLength) {
        maxLength = right - left;
      }
    }

    // If the current character is in the set, remove the character at the left pointer from the set and move the left pointer forward.
    else {
      charSet.delete(str[left]);
      left++;
    }
  }

  // Return the length of the longest substring.
  return maxLength;
}

// This is a function that takes in a string and returns the shortest substring that contains all the characters in a given set.
function shortestSubstringContainingAllCharacters(str, charSet) {
  // Create a map to store the frequency of each character in the set.
  const charMap = {};
  for (let i = 0; i < charSet.length; i++) {
    charMap[charSet[i]] = 0;
  }

  // Initialize two pointers to mark the start and end of the current substring.
  let left = 0;
  let right = 0;

  // Initialize a variable to store the length of the shortest substring.
  let minLength = Infinity;

  // Initialize a variable to store the number of characters in the set that have been found in the current substring.
  let foundChars = 0;

  // Iterate over the string.
  while (right < str.length) {
    // If the current character is in the set, increment the frequency of the character in the map and move the right pointer forward.
    if (charMap[str[right]]) {
      charMap[str[right]]++;
      right++;

      // If the frequency of the current character is greater than or equal to 1, increment the number of characters in the set that have been found.
      if (charMap[str[right]] >= 1) {
        foundChars++;
      }
    }

    // If the number of characters in the set that have been found is equal to the length of the set, the current substring contains all the characters in the set.
    if (foundChars === charSet.length) {
      // While the frequency of the leftmost character in the current substring is greater than 1, decrement the frequency of the character in the map and move the left pointer forward.
      while (charMap[str[left]] > 1) {
        charMap[str[left]]--;
        left++;
      }

      // Update the length of the shortest substring.
      if (right - left < minLength) {
        minLength = right - left;
      }
    }

    // If the number of characters in the set that have been found is less than the length of the set, move the right pointer forward.
    else {
      right++;
    }
  }

  // Return the length of the shortest substring.
  return minLength;
}

// This is a function that takes in a string and returns the longest palindromic substring.
function longestPalindromicSubstring(str) {
  // Initialize a variable to store the longest palindromic substring.
  let longestPalindrome = '';

  // Iterate over the string.
  for (let i = 0; i < str.length; i++) {
    // Expand around the current character.
    let left = i;
    let right = i;
    while (left >= 0 && right < str.length && str[left] === str[right]) {
      if (right - left + 1 > longestPalindrome.length) {
        longestPalindrome = str.substring(left, right + 1);
      }
      left--;
      right++;
    }

    // Expand around the current character and its next character.
    left = i;
    right = i + 1;
    while (left >= 0 && right < str.length && str[left] === str[right]) {
      if (right - left + 1 > longestPalindrome.length) {
        longestPalindrome = str.substring(left, right + 1);
      }
      left--;
      right++;
    }
  }

  // Return the longest palindromic substring.
  return longestPalindrome;
}

// This is a function that takes in an array of numbers and returns the longest increasing subsequence.
function longestIncreasingSubsequence(arr) {
  // Initialize a variable to store the longest increasing subsequence.
  let longestSubsequence = [];

  // Initialize a table to store the length of the longest increasing subsequence ending at each index.
  const dp = new Array(arr.length).fill(1);

  // Iterate over the array.
  for (let i = 1; i < arr.length; i++) {
    // For each element, find the longest increasing subsequence ending at the previous element that is less than the current element.
    for (let j = 0; j < i; j++) {
      if (arr[i] > arr[j] && dp[