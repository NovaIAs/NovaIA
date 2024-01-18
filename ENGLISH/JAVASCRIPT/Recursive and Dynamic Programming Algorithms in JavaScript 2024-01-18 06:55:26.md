```javascript
// Define a function to calculate the nth Fibonacci number using recursion
const fibonacci = (n) => {
  if (n <= 1) {
    return n;
  } else {
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
};

// Define a function to calculate the factorial of a number using recursion
const factorial = (n) => {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
};

// Define a function to check if a number is prime using trial division
const isPrime = (n) => {
  if (n <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      return false;
    }
  }
  return true;
};

// Define a function to find the greatest common divisor of two numbers using the Euclidean algorithm
const gcd = (a, b) => {
  if (b === 0) {
    return a;
  } else {
    return gcd(b, a % b);
  }
};

// Define a function to find the least common multiple of two numbers by multiplying them and dividing by their greatest common divisor
const lcm = (a, b) => {
  return (a * b) / gcd(a, b);
};

// Define a function to sort an array of numbers in ascending order using the merge sort algorithm
const mergeSort = (arr) => {
  if (arr.length <= 1) {
    return arr;
  }
  const mid = Math.floor(arr.length / 2);
  const left = arr.slice(0, mid);
  const right = arr.slice(mid);
  return merge(mergeSort(left), mergeSort(right));
};

// Helper function to merge two sorted arrays into a single sorted array
const merge = (left, right) => {
  const merged = [];
  let i = 0;
  let j = 0;
  while (i < left.length && j < right.length) {
    if (left[i] < right[j]) {
      merged.push(left[i]);
      i++;
    } else {
      merged.push(right[j]);
      j++;
    }
  }
  while (i < left.length) {
    merged.push(left[i]);
    i++;
  }
  while (j < right.length) {
    merged.push(right[j]);
    j++;
  }
  return merged;
};

// Define a function to find the longest common subsequence of two strings using dynamic programming
const longestCommonSubsequence = (str1, str2) => {
  const dp = Array(str1.length + 1).fill(0).map(() => Array(str2.length + 1).fill(0));
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  return dp[str1.length][str2.length];
};

// Define a function to find the shortest common supersequence of two strings using dynamic programming
const shortestCommonSupersequence = (str1, str2) => {
  const dp = Array(str1.length + 1).fill(0).map(() => Array(str2.length + 1).fill(0));
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  let lcs = dp[str1.length][str2.length];
  let scs = str1.length + str2.length - lcs;
  return scs;
};

// Define a function to find the minimum number of coins needed to make a change for a given amount of money using dynamic programming
const coinChange = (coins, amount) => {
  const dp = Array(amount + 1).fill(Infinity);
  dp[0] = 0;
  for (let i = 1; i <= amount; i++) {
    for (let j = 0; j < coins.length; j++) {
      if (i >= coins[j]) {
        dp[i] = Math.min(dp[i], dp[i - coins[j]] + 1);
      }
    }
  }
  return dp[amount] === Infinity ? -1 : dp[amount];
};

// Define a function to find the longest increasing subsequence of an array using dynamic programming
const longestIncreasingSubsequence = (arr) => {
  const dp = Array(arr.length).fill(1);
  for (let i = 1; i < arr.length; i++) {
    for (let j = 0; j < i; j++) {
      if (arr[i] > arr[j]) {
        dp[i] = Math.max(dp[i], dp[j] + 1);
      }
    }
  }
  return Math.max(...dp);
};

// Define a function to find the maximum sum of a contiguous subarray using Kadane's algorithm
const maxSubarraySum = (arr) => {
  let maxSoFar = 0;
  let maxEndingHere = 0;
  for (let i = 0; i < arr.length; i++) {
    maxEndingHere = maxEndingHere + arr[i];
    if (maxSoFar < maxEndingHere) {
      maxSoFar = maxEndingHere;
    }
    if (maxEndingHere < 0) {
      maxEndingHere = 0;
    }
  }
  return maxSoFar;
};

// Define a function to find the minimum number of jumps to reach the end of an array using dynamic programming
const minJumps = (arr) => {
  const dp = Array(arr.length).fill(Infinity);
  dp[0] = 0;