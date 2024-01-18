```javascript
// Function to calculate the factorial of a non-negative integer
function factorial(n) {
  if (n < 0) {
    throw new Error("Factorial is undefined for negative numbers");
  }
  if (n === 0) {
    return 1;
  }
  let result = 1;
  while (n > 1) {
    result *= n;
    n--;
  }
  return result;
}

// Function to find the greatest common divisor (GCD) of two non-negative integers
function gcd(a, b) {
  if (a < 0 || b < 0) {
    throw new Error("GCD is undefined for negative numbers");
  }
  if (b === 0) {
    return a;
  }
  return gcd(b, a % b);
}

// Function to check if a number is prime
function isPrime(n) {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
}

// Function to generate a random integer between a minimum and maximum value
function randomInteger(min, max) {
  if (min > max) {
    throw new Error("Minimum value cannot be greater than maximum value");
  }
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Function to shuffle an array in-place using the Fisher-Yates algorithm
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const randomIndex = randomInteger(0, i);
    const temp = array[i];
    array[i] = array[randomIndex];
    array[randomIndex] = temp;
  }
}

// Function to deep copy an object or array
function deepCopy(obj) {
  if (typeof obj !== "object" || obj === null) {
    return obj;
  }
  if (obj instanceof Array) {
    return obj.map(item => deepCopy(item));
  }
  const copy = {};
  for (const key in obj) {
    copy[key] = deepCopy(obj[key]);
  }
  return copy;
}

// Function to flatten a nested array into a single-level array
function flattenArray(array) {
  const result = [];
  for (let i = 0; i < array.length; i++) {
    const item = array[i];
    if (Array.isArray(item)) {
      result.push(...flattenArray(item));
    } else {
      result.push(item);
    }
  }
  return result;
}

// Function to merge two sorted arrays into a single sorted array
function mergeSortedArrays(arr1, arr2) {
  const mergedArray = [];
  let i1 = 0;
  let i2 = 0;
  while (i1 < arr1.length && i2 < arr2.length) {
    if (arr1[i1] < arr2[i2]) {
      mergedArray.push(arr1[i1]);
      i1++;
    } else {
      mergedArray.push(arr2[i2]);
      i2++;
    }
  }
  while (i1 < arr1.length) {
    mergedArray.push(arr1[i1]);
    i1++;
  }
  while (i2 < arr2.length) {
    mergedArray.push(arr2[i2]);
    i2++;
  }
  return mergedArray;
}

// Function to find the longest common subsequence (LCS) between two strings
function longestCommonSubsequence(str1, str2) {
  const dp = new Array(str1.length + 1).fill(0).map(() => new Array(str2.length + 1).fill(0));
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  let lcs = "";
  let i = str1.length;
  let j = str2.length;
  while (i > 0 && j > 0) {
    if (str1[i - 1] === str2[j - 1]) {
      lcs = str1[i - 1] + lcs;
      i--;
      j--;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      i--;
    } else {
      j--;
    }
  }
  return lcs;
}

// Function to find the shortest common supersequence (SCS) between two strings
function shortestCommonSupersequence(str1, str2) {
  const dp = new Array(str1.length + 1).fill(0).map(() => new Array(str2.length + 1).fill(0));
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        dp[i][j] = 1 + dp[i - 1][j - 1];
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  let scs = "";
  let i = str1.length;
  let j = str2.length;
  while (i > 0 || j > 0) {
    if (str1[i - 1] === str2[j - 1]) {
      scs = str1[i - 1] + scs;
      i--;
      j--;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      scs = str1[i - 1] + scs;
      i--;
    } else {
      scs = str2[j - 1] + scs;
      j--;
    }
  }
  return scs;
}

// Function to find the longest palindrome substring in a string
function longestPalindrome(str) {
  let longest = "";
  for (let i = 0; i < str.length; i++) {
    for (let j = i + 1; j <= str.length; j++) {
      const substring = str.substring(i, j);
      if (substring === substring.split("").reverse().join("") && substring.length > longest.length) {
        longest = substring;
      }
    }
  }
  return longest;
}

// Function to check if a string is a palindrome
function isPalindrome(str) {
  return str === str.split("").reverse().join("");
}

// Function to reverse a string
function reverseString(str) {
  let reversed = "";
  for (let i = str.length - 1; i >= 0; i--) {
    reversed += str[i];
  }
  return reversed;
}
```