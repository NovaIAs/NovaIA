```javascript
// 1. Function to Calculate Fibonacci Numbers
// This function takes a number n and returns its nth Fibonacci number.
function fibonacci(n) {
  if (n <= 1) {
    return n;
  } else {
    return fibonacci(n - 1) + fibonacci(n - 2);
  }
}

// 2. Function to Find the GCD (Greatest Common Divisor) of Two Numbers
// This function takes two numbers a and b and returns their GCD using the Euclidean Algorithm.
function gcd(a, b) {
  if (b === 0) {
    return a;
  } else {
    return gcd(b, a % b);
  }
}

// 3. Function to Generate Random Numbers
// This function generates a random number between min and max (inclusive).
function randomInteger(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// 4. Function to Check if a Number is Prime
// This function takes a number n and returns true if it's prime and false otherwise.
function isPrime(n) {
  if (n <= 1) {
    return false;
  } else if (n <= 3) {
    return true;
  } else if (n % 2 === 0 || n % 3 === 0) {
    return false;
  }

  for (let i = 5; i * i <= n; i += 6) {
    if (n % i === 0 || n % (i + 2) === 0) {
      return false;
    }
  }

  return true;
}

// 5. Function to Find the Factors of a Number
// This function takes a number n and returns an array containing all of its factors.
function factors(n) {
  const factors = [];
  for (let i = 1; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      factors.push(i);
      if (n / i !== i) {
        factors.push(n / i);
      }
    }
  }

  return factors;
}

// 6. Function to Sort an Array of Numbers in Ascending Order
// This function takes an array of numbers and returns a new array containing the sorted numbers in ascending order.
function sortAscending(array) {
  return array.sort((a, b) => a - b);
}

// 7. Function to Sort an Array of Numbers in Descending Order
// This function takes an array of numbers and returns a new array containing the sorted numbers in descending order.
function sortDescending(array) {
  return array.sort((a, b) => b - a);
}

// 8. Function to Find the Largest Number in an Array
// This function takes an array of numbers and returns the largest number in the array.
function findMax(array) {
  let max = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > max) {
      max = array[i];
    }
  }

  return max;
}

// 9. Function to Find the Smallest Number in an Array
// This function takes an array of numbers and returns the smallest number in the array.
function findMin(array) {
  let min = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < min) {
      min = array[i];
    }
  }

  return min;
}

// 10. Function to Calculate the Average of an Array of Numbers
// This function takes an array of numbers and returns the average of the numbers in the array.
function calculateAverage(array) {
  let sum = 0;
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }

  return sum / array.length;
}

// 11. Function to Find the Median of an Array of Numbers
// This function takes an array of numbers and returns the median of the numbers in the array.
function findMedian(array) {
  array.sort((a, b) => a - b);
  const middleIndex = Math.floor(array.length / 2);
  if (array.length % 2 === 0) {
    return (array[middleIndex] + array[middleIndex - 1]) / 2;
  } else {
    return array[middleIndex];
  }
}

// 12. Function to Find the Mode of an Array of Numbers
// This function takes an array of numbers and returns the mode of the numbers in the array
// (the number that occurs most frequently).
function findMode(array) {
  const counts = {};
  for (let i = 0; i < array.length; i++) {
    const number = array[i];
    if (!counts[number]) {
      counts[number] = 0;
    }

    counts[number]++;
  }

  let maxCount = 0;
  let mode;
  for (const number in counts) {
    if (counts[number] > maxCount) {
      maxCount = counts[number];
      mode = number;
    }
  }

  return mode;
}
```

Explanation:

1. The `fibonacci` function calculates Fibonacci numbers using recursion.

2. The `gcd` function finds the greatest common divisor of two numbers using the Euclidean Algorithm.

3. The `randomInteger` function generates a random integer between a specified range.

4. The `isPrime` function checks if a given number is prime.

5. The `factors` function finds all the factors of a given number.

6. The `sortAscending` and `sortDescending` functions sort an array of numbers in ascending and descending order, respectively.

7. The `findMax` and `findMin` functions find the largest and smallest numbers in an array, respectively.

8. The `calculateAverage` function calculates the average of an array of numbers.

9. The `findMedian` function finds the median of an array of numbers.

10. The `findMode` function finds the mode of an array of numbers.