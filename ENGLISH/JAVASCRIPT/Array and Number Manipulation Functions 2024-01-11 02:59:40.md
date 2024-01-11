```javascript
// Define a function to calculate the factorial of a number
function factorial(n) {
  if (n === 0) {
    return 1;
  } else {
    return n * factorial(n - 1);
  }
}

// Define a function to generate a random number between two numbers
function randomNumber(min, max) {
  return Math.floor(Math.random() * (max - min + 1) + min);
}

// Define a function to shuffle an array
function shuffleArray(array) {
  for (let i = array.length - 1; i > 0; i--) {
    const j = randomNumber(0, i);
    [array[i], array[j]] = [array[j], array[i]];
  }
}

// Define a function to find the maximum value in an array
function max(array) {
  let max = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] > max) {
      max = array[i];
    }
  }
  return max;
}

// Define a function to find the minimum value in an array
function min(array) {
  let min = array[0];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < min) {
      min = array[i];
    }
  }
  return min;
}

// Define a function to find the average value in an array
function average(array) {
  let sum = 0;
  for (let i = 0; i < array.length; i++) {
    sum += array[i];
  }
  return sum / array.length;
}

// Define a function to find the median value in an array
function median(array) {
  // Sort the array in ascending order
  array.sort((a, b) => a - b);
  // Find the middle index of the array
  const middleIndex = Math.floor(array.length / 2);
  // If the array has an even number of elements, return the average of the two middle elements
  if (array.length % 2 === 0) {
    return (array[middleIndex] + array[middleIndex - 1]) / 2;
  }
  // If the array has an odd number of elements, return the middle element
  else {
    return array[middleIndex];
  }
}

// Define a function to find the mode value in an array
function mode(array) {
  // Create an object to store the frequency of each element in the array
  const frequency = {};
  for (let i = 0; i < array.length; i++) {
    if (frequency[array[i]]) {
      frequency[array[i]]++;
    } else {
      frequency[array[i]] = 1;
    }
  }

  // Find the maximum frequency value
  const maxFrequency = max(Object.values(frequency));

  // Create an array to store the mode values
  const modes = [];
  for (const key in frequency) {
    if (frequency[key] === maxFrequency) {
      modes.push(Number(key));
    }
  }

  // Return the mode values
  return modes;
}

// Define a function to find the standard deviation of an array
function standardDeviation(array) {
  // Calculate the mean of the array
  const mean = average(array);

  // Calculate the variance of the array
  const variance = array.reduce((acc, curr) => acc + Math.pow(curr - mean, 2), 0) / (array.length - 1);

  // Calculate the standard deviation of the array
  const standardDeviation = Math.sqrt(variance);

  // Return the standard deviation
  return standardDeviation;
}

// Define a function to find the covariance of two arrays
function covariance(array1, array2) {
  // Check if the arrays have the same length
  if (array1.length !== array2.length) {
    throw new Error("Arrays must have the same length.");
  }

  // Calculate the mean of the first array
  const mean1 = average(array1);

  // Calculate the mean of the second array
  const mean2 = average(array2);

  // Calculate the covariance of the two arrays
  const covariance = array1.reduce((acc, curr, i) => acc + (curr - mean1) * (array2[i] - mean2), 0) / (array1.length - 1);

  // Return the covariance
  return covariance;
}

// Define a function to find the correlation coefficient of two arrays
function correlationCoefficient(array1, array2) {
  // Check if the arrays have the same length
  if (array1.length !== array2.length) {
    throw new Error("Arrays must have the same length.");
  }

  // Calculate the covariance of the two arrays
  const covariance = covariance(array1, array2);

  // Calculate the standard deviation of the first array
  const standardDeviation1 = standardDeviation(array1);

  // Calculate the standard deviation of the second array
  const standardDeviation2 = standardDeviation(array2);

  // Calculate the correlation coefficient of the two arrays
  const correlationCoefficient = covariance / (standardDeviation1 * standardDeviation2);

  // Return the correlation coefficient
  return correlationCoefficient;
}

// Define a function to find the linear regression line of two arrays
function linearRegressionLine(array1, array2) {
  // Check if the arrays have the same length
  if (array1.length !== array2.length) {
    throw new Error("Arrays must have the same length.");
  }

  // Calculate the mean of the first array
  const mean1 = average(array1);

  // Calculate the mean of the second array
  const mean2 = average(array2);

  // Calculate the covariance of the two arrays
  const covariance = covariance(array1, array2);

  // Calculate the variance of the first array
  const variance1 = standardDeviation(array1) ** 2;

  // Calculate the slope of the linear regression line
  const slope = covariance / variance1;

  // Calculate the y-intercept of the linear regression line
  const yIntercept = mean2 - slope * mean1;

  // Return the linear regression line
  return { slope, yIntercept };
}
```