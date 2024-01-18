```javascript
// Create a function to generate a random number between two values
function getRandomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

// Create a function to generate a random string of a given length
function getRandomString(length) {
  const characters = 'ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789';
  let result = '';
  for (let i = 0; i < length; i++) {
    result += characters[getRandomNumber(0, characters.length - 1)];
  }
  return result;
}

// Create a function to shuffle an array
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = getRandomNumber(0, i);
    const temp = array[i];
    array[i] = array[j];
    array[j] = temp;
  }
  return array;
}

// Create a function to deep clone an object
function deepCloneObject(object) {
  if (Array.isArray(object)) {
    return object.map(deepCloneObject);
  } else if (typeof object === 'object' && object !== null) {
    const result = {};
    for (const key in object) {
      result[key] = deepCloneObject(object[key]);
    }
    return result;
  } else {
    return object;
  }
}

// Create a function to calculate the factorial of a number
function factorial(n) {
  if (n < 0) {
    throw new Error('Cannot calculate the factorial of a negative number');
  }
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Create a function to find the greatest common divisor of two numbers
function greatestCommonDivisor(a, b) {
  if (b === 0) {
    return a;
  } else {
    return greatestCommonDivisor(b, a % b);
  }
}

// Create a function to find the least common multiple of two numbers
function leastCommonMultiple(a, b) {
  return (a * b) / greatestCommonDivisor(a, b);
}

// Create a function to convert a number to binary
function toBinary(n) {
  if (n === 0) {
    return '0';
  } else {
    return toBinary(Math.floor(n / 2)) + (n % 2).toString();
  }
}

// Create a function to convert a number to hexadecimal
function toHexadecimal(n) {
  const hexadecimalDigits = '0123456789ABCDEF';
  if (n === 0) {
    return '0';
  } else {
    return toHexadecimal(Math.floor(n / 16)) + hexadecimalDigits[n % 16];
  }
}

// Create a function to check if a number is prime
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

// Create a function to find the prime factors of a number
function primeFactors(n) {
  const primeFactors = [];
  for (let i = 2; i <= Math.sqrt(n); i++) {
    while (n % i === 0) {
      primeFactors.push(i);
      n /= i;
    }
  }
  if (n > 1) {
    primeFactors.push(n);
  }
  return primeFactors;
}

// Create a function to find the sum of all the divisors of a number
function sumOfDivisors(n) {
  let sum = 0;
  for (let i = 1; i <= Math.sqrt(n); i++) {
    if (n % i === 0) {
      sum += i;
      if (i !== n / i) {
        sum += n / i;
      }
    }
  }
  return sum;
}

// Create a function to check if a number is perfect
function isPerfect(n) {
  return sumOfDivisors(n) === 2 * n;
}

// Create a function to find the smallest positive integer that is evenly divisible by all the numbers from 1 to n
function smallestMultiple(n) {
  let smallestMultiple = 1;
  for (let i = 2; i <= n; i++) {
    smallestMultiple = leastCommonMultiple(smallestMultiple, i);
  }
  return smallestMultiple;
}

// Create a function to find the largest palindrome that is the product of two n-digit numbers
function largestPalindromeProduct(n) {
  let largestPalindrome = 0;
  for (let i = 10**(n-1); i < 10**n; i++) {
    for (let j = 10**(n-1); j < 1