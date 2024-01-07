```javascript
// Create a function that takes an array of numbers and returns the sum of all the numbers in the array.
function sumArray(numbers) {
  // Check if the input is valid.
  if (!Array.isArray(numbers)) {
    throw new Error("Invalid input: the input must be an array.");
  }

  // Initialize the sum to 0.
  let sum = 0;

  // Iterate over the array and add each number to the sum.
  for (const number of numbers) {
    // Check if the current element is a number.
    if (typeof number !== "number") {
      throw new Error("Invalid input: the array can only contain numbers.");
    }

    // Add the current number to the sum.
    sum += number;
  }

  // Return the sum of all the numbers in the array.
  return sum;
}

// Create a function that takes a string and returns the number of words in the string.
function countWords(string) {
  // Check if the input is valid.
  if (typeof string !== "string") {
    throw new Error("Invalid input: the input must be a string.");
  }

  // Split the string into an array of words.
  const words = string.split(" ");

  // Return the number of words in the array.
  return words.length;
}

// Create a function that takes a string and returns the number of vowels in the string.
function countVowels(string) {
  // Check if the input is valid.
  if (typeof string !== "string") {
    throw new Error("Invalid input: the input must be a string.");
  }

  // Create a regular expression to match vowels.
  const vowelRegex = /[aeiou]/gi;

  // Find all the vowels in the string.
  const vowels = string.match(vowelRegex);

  // Return the number of vowels in the string.
  return vowels.length;
}

// Create a function that takes a string and returns the most frequently occurring character in the string.
function findMostFrequentCharacter(string) {
  // Check if the input is valid.
  if (typeof string !== "string") {
    throw new Error("Invalid input: the input must be a string.");
  }

  // Create an object to store the character frequencies.
  const characterFrequencies = {};

  // Iterate over the string and count the frequency of each character.
  for (const character of string) {
    // Check if the character is already in the object.
    if (characterFrequencies[character]) {
      // Increment the frequency of the character.
      characterFrequencies[character]++;
    } else {
      // Add the character to the object with a frequency of 1.
      characterFrequencies[character] = 1;
    }
  }

  // Find the character with the highest frequency.
  let mostFrequentCharacter = "";
  let highestFrequency = 0;
  for (const character in characterFrequencies) {
    if (characterFrequencies[character] > highestFrequency) {
      mostFrequentCharacter = character;
      highestFrequency = characterFrequencies[character];
    }
  }

  // Return the most frequently occurring character.
  return mostFrequentCharacter;
}

// Create a function that takes a string and returns the longest word in the string.
function findLongestWord(string) {
  // Check if the input is valid.
  if (typeof string !== "string") {
    throw new Error("Invalid input: the input must be a string.");
  }

  // Split the string into an array of words.
  const words = string.split(" ");

  // Find the longest word in the array.
  let longestWord = "";
  for (const word of words) {
    if (word.length > longestWord.length) {
      longestWord = word;
    }
  }

  // Return the longest word.
  return longestWord;
}

// Create a function that takes an array of objects and returns a new array containing only the objects that have a certain property.
function filterObjectsByProperty(objects, property, value) {
  // Check if the input is valid.
  if (!Array.isArray(objects)) {
    throw new Error("Invalid input: the input must be an array.");
  }

  if (typeof property !== "string") {
    throw new Error("Invalid input: the property must be a string.");
  }

  if (typeof value === "undefined") {
    throw new Error("Invalid input: the value must be defined.");
  }

  // Create a new array to store the filtered objects.
  const filteredObjects = [];

  // Iterate over the array of objects.
  for (const object of objects) {
    // Check if the object has the specified property.
    if (object.hasOwnProperty(property)) {
      // Check if the value of the property matches the specified value.
      if (object[property] === value) {
        // Add the object to the array of filtered objects.
        filteredObjects.push(object);
      }
    }
  }

  // Return the array of filtered objects.
  return filteredObjects;
}

// Create a function that takes a string and returns a new string with all the vowels removed.
function removeVowels(string) {
  // Check if the input is valid.
  if (typeof string !== "string") {
    throw new Error("Invalid input: the input must be a string.");
  }

  // Create a regular expression to match vowels.
  const vowelRegex = /[aeiou]/gi;

  // Replace all the vowels in the string with an empty string.
  const newString = string.replace(vowelRegex, "");

  // Return the new string with all the vowels removed.
  return newString;
}

// Create a function that takes two strings and returns a new string that is a combination of the two strings.
function combineStrings(string1, string2) {
  // Check if the input is valid.
  if (typeof string1 !== "string") {
    throw new Error("Invalid input: the first string must be a string.");
  }

  if (typeof string2 !== "string") {
    throw new Error("Invalid input: the second string must be a string.");
  }

  // Create a new string that is a combination of the two strings.
  const newString = string1 + string2;

  // Return the new string.
  return newString;
}

// Create a function that takes an array of numbers and returns a new array containing the square of each number.
function squareNumbers(numbers) {
  // Check if the input is valid.
  if (!Array.isArray(numbers)) {
    throw new Error("Invalid input: the input must be an array.");
  }

  // Create a new array to store the squared numbers.
  const squaredNumbers = [];

  // Iterate over the array of numbers.
  for (const number of numbers) {
    // Check if the current element is a number.
    if (typeof number !== "number") {
      throw new Error("Invalid input: the array can only contain numbers.");
    }

    // Square the current number.
    const squaredNumber = number * number;

    // Add the squared number to the array of squared numbers.
    squaredNumbers.push(squaredNumber);
  }

  // Return the array of squared numbers.
  return squaredNumbers;
}

// Create a function that takes an array of strings and returns a new array containing the length of each string.
function