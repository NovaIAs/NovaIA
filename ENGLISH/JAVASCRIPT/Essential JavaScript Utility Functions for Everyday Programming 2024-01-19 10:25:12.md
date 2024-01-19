Sure, here is an example of a complex and differentiated JavaScript code:

```
// This function takes a two-dimensional array and returns a new array with the elements of the main diagonal.
const getMainDiagonal = (matrix) => {
  if (!Array.isArray(matrix) || matrix.length === 0 || matrix[0].length === 0) {
    throw new Error("Invalid input matrix.");
  }

  const n = matrix.length;
  const diagonal = [];
  for (let i = 0; i < n; i++) {
    diagonal.push(matrix[i][i]);
  }
  return diagonal;
};

// This function takes a two-dimensional array and returns a new array with the elements of the secondary diagonal.
const getSecondaryDiagonal = (matrix) => {
  if (!Array.isArray(matrix) || matrix.length === 0 || matrix[0].length === 0) {
    throw new Error("Invalid input matrix.");
  }

  const n = matrix.length;
  const diagonal = [];
  for (let i = 0, j = n - 1; i < n; i++, j--) {
    diagonal.push(matrix[i][j]);
  }
  return diagonal;
};

// This function takes two arrays and returns a new array with the elements of the first array repeated the number of times specified in the second array.
const repeatElements = (array1, array2) => {
  if (!Array.isArray(array1) || !Array.isArray(array2) || array1.length !== array2.length) {
    throw new Error("Invalid input arrays.");
  }

  const result = [];
  for (let i = 0; i < array1.length; i++) {
    for (let j = 0; j < array2[i]; j++) {
      result.push(array1[i]);
    }
  }
  return result;
};

// This function takes an array and returns a new array with the elements sorted in descending order.
const sortDescending = (array) => {
  if (!Array.isArray(array)) {
    throw new Error("Invalid input array.");
  }

  return array.sort((a, b) => b - a);
};

// This function takes a string and returns a new string with the first and last characters swapped.
const swapFirstAndLastChars = (string) => {
  if (typeof string !== "string" || string.length === 0) {
    throw new Error("Invalid input string.");
  }

  const firstChar = string[0];
  const lastChar = string[string.length - 1];
  const middleChars = string.substring(1, string.length - 1);
  return lastChar + middleChars + firstChar;
};

// This function takes a number and returns the sum of its digits.
const sumDigits = (number) => {
  if (typeof number !== "number" || isNaN(number)) {
    throw new Error("Invalid input number.");
  }

  const stringNumber = number.toString();
  let sum = 0;
  for (let i = 0; i < stringNumber.length; i++) {
    sum += parseInt(stringNumber[i]);
  }
  return sum;
};

// This function takes a string and returns a new string with the words reversed.
const reverseWords = (string) => {
  if (typeof string !== "string" || string.length === 0) {
    throw new Error("Invalid input string.");
  }

  const words = string.split(" ");
  const reversedWords = words.reverse();
  return reversedWords.join(" ");
};

// This function takes a number and returns true if it is a palindrome, and false otherwise.
const isPalindrome = (number) => {
  if (typeof number !== "number" || isNaN(number)) {
    throw new Error("Invalid input number.");
  }

  const stringNumber = number.toString();
  return stringNumber === stringNumber.split("").reverse().join("");
};

// This function takes an array of numbers and returns the median.
const median = (numbers) => {
  if (!Array.isArray(numbers) || numbers.length === 0) {
    throw new Error("Invalid input array.");
  }

  numbers.sort((a, b) => a - b);
  const n = numbers.length;
  if (n % 2 === 0) {
    return (numbers[n / 2 - 1] + numbers[n / 2]) / 2;
  } else {
    return numbers[Math.floor(n / 2)];
  }
};
```

This code contains a collection of different functions, each performing a specific task. The functions cover a wide range of operations, including matrix manipulation, array manipulation, string manipulation, and number manipulation. The functions are well-documented with comments explaining their purpose and input/output. Additionally, the code is written in a modular and reusable manner, making it easy to integrate into larger projects or use in different contexts.