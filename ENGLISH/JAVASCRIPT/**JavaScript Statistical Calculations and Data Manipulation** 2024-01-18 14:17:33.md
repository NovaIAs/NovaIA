```javascript
// Recursive function to find the factorial of a number
function factorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Function to generate a random number between a minimum and maximum value
function randomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1)) + min;
}

// Function to shuffle an array
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = randomNumber(0, i);
    const temp = array[i];
    array[i] = array[j];
    array[j] = temp;
  }

  return array;
}

// Function to find the maximum value in an array
function maxValue(array) {
  let max = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > max) {
      max = array[i];
    }
  }

  return max;
}

// Function to find the minimum value in an array
function minValue(array) {
  let min = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < min) {
      min = array[i];
    }
  }

  return min;
}

// Function to find the sum of an array
function sumArray(array) {
  let sum = 0;
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }

  return sum;
}

// Function to find the average of an array
function averageArray(array) {
  const sum = sumArray(array);
  const average = sum / array.length;

  return average;
}

// Function to find the median of an array
function medianArray(array) {
  const sortedArray = array.sort((a, b) => a - b);
  const middleIndex = Math.floor(sortedArray.length / 2);

  if (sortedArray.length % 2 === 0) {
    const median = (sortedArray[middleIndex] + sortedArray[middleIndex - 1]) / 2;
    return median;
  } else {
    return sortedArray[middleIndex];
  }
}

// Function to find the mode of an array
function modeArray(array) {
  const counts = {};
  const values = [];

  for (let i = 0; i < array.length; i++) {
    const value = array[i];
    if (counts[value]) {
      counts[value]++;
    } else {
      counts[value] = 1;
      values.push(value);
    }
  }

  const maxCount = Math.max(...Object.values(counts));
  const modes = values.filter((value) => counts[value] === maxCount);

  return modes;
}

// Function to find the range of an array
function rangeArray(array) {
  const max = maxValue(array);
  const min = minValue(array);

  const range = max - min;

  return range;
}

// Function to find the variance of an array
function varianceArray(array) {
  const mean = averageArray(array);

  let squaredDifferences = 0;
  for (let i = 0; i < array.length; i++) {
    const difference = array[i] - mean;
    squaredDifferences += difference * difference;
  }

  const variance = squaredDifferences / (array.length - 1);

  return variance;
}

// Function to find the standard deviation of an array
function standardDeviationArray(array) {
  const variance = varianceArray(array);

  const standardDeviation = Math.sqrt(variance);

  return standardDeviation;
}

// Function to find the quartiles of an array
function quartilesArray(array) {
  const sortedArray = array.sort((a, b) => a - b);

  const q1 = medianArray(sortedArray.slice(0, Math.floor(sortedArray.length / 2)));
  const q2 = medianArray(sortedArray);
  const q3 = medianArray(sortedArray.slice(Math.ceil(sortedArray.length / 2)));

  return [q1, q2, q3];
}
```

This code provides a collection of commonly used functions for statistical calculations and data manipulation in JavaScript. The functions cover a wide range of operations, including factorial calculation, random number generation, array shuffling, finding maximum and minimum values, summing arrays, calculating averages, medians, modes, ranges, variances, and standard deviations. It also includes functions for finding quartiles in an array. This comprehensive code offers a versatile toolset for data analysis and statistical computations in JavaScript.