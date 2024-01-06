```javascript
// This function takes in a string and returns an array of all the unique words in the string.
function getUniqueWords(str) {
  // Convert the string to lowercase and split it into an array of words.
  const words = str.toLowerCase().split(" ");

  // Create a set to store the unique words.
  const uniqueWords = new Set();

  // Iterate over the array of words and add each word to the set if it is not already there.
  for (let word of words) {
    uniqueWords.add(word);
  }

  // Convert the set of unique words back into an array and return it.
  return Array.from(uniqueWords);
}

// This function takes in an array of numbers and returns the sum of all the numbers in the array.
function sumArray(arr) {
  // Initialize the sum to 0.
  let sum = 0;

  // Iterate over the array and add each number to the sum.
  for (let num of arr) {
    sum += num;
  }

  // Return the sum.
  return sum;
}

// This function takes in two arrays of numbers and returns a new array containing the elements that are common to both arrays.
function findCommonElements(arr1, arr2) {
  // Create a set to store the common elements.
  const commonElements = new Set();

  // Iterate over the first array and add each element to the set.
  for (let element of arr1) {
    commonElements.add(element);
  }

  // Iterate over the second array and check if each element is in the set. If it is, add it to the list of common elements.
  for (let element of arr2) {
    if (commonElements.has(element)) {
      commonElements.add(element);
    }
  }

  // Convert the set of common elements back into an array and return it.
  return Array.from(commonElements);
}

// This function takes in a string and returns a new string with all the vowels removed.
function removeVowels(str) {
  // Create a regular expression to match vowels.
  const vowelRegex = /[aeiou]/gi;

  // Replace all the vowels in the string with an empty string.
  const newStr = str.replace(vowelRegex, "");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the consonants removed.
function removeConsonants(str) {
  // Create a regular expression to match consonants.
  const consonantRegex = /[^aeiou]/gi;

  // Replace all the consonants in the string with an empty string.
  const newStr = str.replace(consonantRegex, "");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the punctuation removed.
function removePunctuation(str) {
  // Create a regular expression to match punctuation.
  const punctuationRegex = /[\.,:;!?"]/g;

  // Replace all the punctuation in the string with an empty string.
  const newStr = str.replace(punctuationRegex, "");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the whitespace removed.
function removeWhitespace(str) {
  // Create a regular expression to match whitespace.
  const whitespaceRegex = /\s/g;

  // Replace all the whitespace in the string with an empty string.
  const newStr = str.replace(whitespaceRegex, "");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the characters in reverse order.
function reverseString(str) {
  // Split the string into an array of characters.
  const chars = str.split("");

  // Reverse the array of characters.
  chars.reverse();

  // Join the array of characters back into a string.
  const newStr = chars.join("");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the characters in alphabetical order.
function sortString(str) {
  // Split the string into an array of characters.
  const chars = str.split("");

  // Sort the array of characters in alphabetical order.
  chars.sort();

  // Join the array of characters back into a string.
  const newStr = chars.join("");

  // Return the new string.
  return newStr;
}

// This function takes in a string and returns a new string with all the characters in the opposite case.
function changeCase(str) {
  // Create a new string to store the result