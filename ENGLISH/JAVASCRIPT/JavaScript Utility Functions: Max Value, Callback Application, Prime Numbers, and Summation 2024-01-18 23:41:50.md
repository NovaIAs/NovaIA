```javascript
// Higher-Order Function to find the max of an array of numbers
const findMax = (numbers) => {
  let max = Number.MIN_SAFE_INTEGER; // Initialize max to the smallest possible JavaScript number
  numbers.forEach((number) => {
    if (number > max) {
      max = number;
    }
  });
  return max;
};

// Function that takes a callback function and an array and applies the callback to each element in the array
const applyCallback = (callback, array) => {
  const results = [];
  array.forEach((element) => {
    results.push(callback(element));
  });
  return results;
};

// Function to check if a number is prime
const isPrime = (number) => {
  if (number <= 1) {
    return false;
  }
  for (let i = 2; i <= Math.sqrt(number); i++) {
    if (number % i === 0) {
      return false;
    }
  }
  return true;
};

// Function to generate an array of prime numbers up to a given limit
const generatePrimes = (limit) => {
  const primes = [];
  for (let i = 2; i <= limit; i++) {
    if (isPrime(i)) {
      primes.push(i);
    }
  }
  return primes;
};

// Function to find the sum of an array of numbers
const sumArray = (numbers) => {
  let sum = 0;
  numbers.forEach((number) => {
    sum += number;
  });
  return sum;
};

// Main function
const main = () => {
  // Find the max of an array of numbers
  const numbers = [1, 3, 5, 7, 9, 11, 13];
  const max = findMax(numbers);
  console.log("Max:", max);

  // Apply a callback function to each element in an array
  const doubledNumbers = applyCallback((number) => number * 2, numbers);
  console.log("Doubled numbers:", doubledNumbers);

  // Generate an array of prime numbers up to a given limit
  const primes = generatePrimes(100);
  console.log("Prime numbers up to 100:", primes);

  // Find the sum of an array of numbers
  const sum = sumArray(numbers);
  console.log("Sum:", sum);
};

main();
```

Explanation:

1. Higher-Order Function `findMax`:
   - This function takes an array of numbers as an argument and returns the maximum value in the array.
   - It uses a loop to iterate over the array and compares each element with the current maximum value.
   - When it finds a larger value, it updates the maximum value.

2. Function `applyCallback`:
   - This function takes a callback function and an array as arguments.
   - It applies the callback function to each element in the array and returns an array of the results.
   - This function is used to apply a common operation to all elements in an array.

3. Function `isPrime`:
   - This function checks if a given number is prime.
   - It uses a loop to check if the number is divisible by any integer from 2 to the square root of the number.
   - If it finds a divisor, it returns `false`, indicating that the number is not prime. If it completes the loop without finding a divisor, it returns `true`.

4. Function `generatePrimes`:
   - This function generates an array of prime numbers up to a given limit.
   - It uses a loop to iterate from 2 to the given limit and checks if each number is prime using the `isPrime` function.
   - If a number is prime, it is added to the array of prime numbers.

5. Function `sumArray`:
   - This function takes an array of numbers as an argument and returns the sum of all the numbers in the array.
   - It uses a loop to iterate over the array and adds each element to a running total.

6. Main Function:
   - The `main` function is the entry point of the program.
   - It calls the other functions to demonstrate their functionality.
   - It finds the maximum value in an array, applies a callback to each element in an array, generates an array of prime numbers, and finds the sum of an array.