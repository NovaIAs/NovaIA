```javascript
// Recursive function to calculate the Fibonacci sequence
const fibonacci = (n) => {
  if (n <= 1) {
    return n;
  } else {
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
};

// Function to check if a number is prime
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

// Function to find all the prime numbers up to a given number
const findPrimes = (n) => {
  const primes = [];
  for (let i = 2; i <= n; i++) {
    if (isPrime(i)) {
      primes.push(i);
    }
  }
  return primes;
};

// Function to sort an array of numbers in ascending order
const sortAscending = (arr) => {
  return arr.sort((a, b) => a - b);
};

// Function to sort an array of numbers in descending order
const sortDescending = (arr) => {
  return arr.sort((a, b) => b - a);
};

// Function to find the maximum value in an array of numbers
const findMax = (arr) => {
  return Math.max(...arr);
};

// Function to find the minimum value in an array of numbers
const findMin = (arr) => {
  return Math.min(...arr);
};

// Function to calculate the average of an array of numbers
const findAverage = (arr) => {
  return arr.reduce((a, b) => a + b, 0) / arr.length;
};

// Function to find the median of an array of numbers
const findMedian = (arr) => {
  const sortedArr = sortAscending(arr);
  const mid = Math.floor(sortedArr.length / 2);
  if (sortedArr.length % 2 === 0) {
    return (sortedArr[mid] + sortedArr[mid - 1]) / 2;
  } else {
    return sortedArr[mid];
  }
};

// Function to find the mode of an array of numbers
const findMode = (arr) => {
  const counts = {};
  let maxCount = 0;
  let mode;
  for (const num of arr) {
    counts[num] = (counts[num] || 0) + 1;
    if (counts[num] > maxCount) {
      maxCount = counts[num];
      mode = num;
    }
  }
  return mode;
};

// Function to find the range of an array of numbers
const findRange = (arr) => {
  return findMax(arr) - findMin(arr);
};

// Function to find the variance of an array of numbers
const findVariance = (arr) => {
  const average = findAverage(arr);
  const deviations = [];
  for (const num of arr) {
    deviations.push(Math.pow(num - average, 2));
  }
  return findAverage(deviations);
};

// Function to find the standard deviation of an array of numbers
const findStandardDeviation = (arr) => {
  return Math.sqrt(findVariance(arr));
};
```

This code is a collection of commonly used mathematical functions in JavaScript. It includes functions for calculating the Fibonacci sequence, checking if a number is prime, finding all the prime numbers up to a given number, sorting an array of numbers in ascending or descending order, finding the maximum, minimum, average, median, mode, range, variance, and standard deviation of an array of numbers. This code can be used for various mathematical and statistical calculations and is useful for data analysis, machine learning, and other applications.