```javascript
// This code defines a function called 'factorial' that calculates the factorial of a given number.
function factorial(n) {
  // If the given number is less than or equal to 0, we return 1 as the factorial of 0 and negative numbers is undefined.
  if (n <= 0) {
    return 1;
  }

  // Initialize a variable called 'result' to 1. This variable will store the factorial value as we calculate it.
  let result = 1;

  // Start a loop from 'i' = 1 up to 'i' <= 'n'.
  for (let i = 1; i <= n; i++) {
    // Inside the loop, we multiply 'result' by 'i' and store the result back in 'result'.
    result *= i;
  }

  // Finally, we return the calculated factorial value stored in 'result'.
  return result;
}

// This code defines a function called 'isPrime' that checks if a given number is prime.
function isPrime(n) {
  // If the given number is less than or equal to 1, it is not prime.
  if (n <= 1) {
    return false;
  }

  // Initialize a variable called 'limit' to the square root of 'n'.
  // We only need to check divisors up to the square root of 'n' to determine if it is prime.
  const limit = Math.sqrt(n);

  // Start a loop from 'i' = 2 up to 'i' <= 'limit'.
  for (let i = 2; i <= limit; i++) {
    // If 'n' is divisible by 'i' without remainder, it is not prime.
    if (n % i === 0) {
      return false;
    }
  }

  // If 'n' was not divisible by any number up to its square root, it is prime.
  return true;
}

// This code defines a function called 'fibonacci' that generates the Fibonacci sequence up to a given number.
function fibonacci(n) {
  // If the given number is less than or equal to 0, return an empty array.
  if (n <= 0) {
    return [];
  }

  // Initialize an array called 'sequence' to store the Fibonacci sequence.
  const sequence = [0, 1];

  // Start a loop from 'i' = 2 up to 'i' < 'n'.
  for (let i = 2; i < n; i++) {
    // Calculate the next Fibonacci number by adding the last two numbers in the sequence.
    const nextNumber = sequence[i - 1] + sequence[i - 2];

    // Add the next Fibonacci number to the sequence array.
    sequence.push(nextNumber);
  }

  // Finally, return the generated Fibonacci sequence.
  return sequence;
}

// This code defines a function called 'caesarCipher' that encrypts a given string using the Caesar cipher.
function caesarCipher(string, shift) {
  // Convert the given string to uppercase for simplicity.
  string = string.toUpperCase();

  // Create an empty string called 'encrypted' to store the encrypted message.
  let encrypted = "";

  // Start a loop through each character in the string.
  for (let i = 0; i < string.length; i++) {
    // Get the ASCII code of the current character.
    const charCode = string.charCodeAt(i);

    // Check if the current character is an alphabet (A-Z).
    if (charCode >= 65 && charCode <= 90) {
      // Shift the character's ASCII code by the given shift amount.
      const shiftedCharCode = charCode + shift;

      // Handle the case when shifting goes beyond the alphabet's range (Z).
      if (shiftedCharCode > 90) {
        // Wrap around to the beginning of the alphabet (A).
        shiftedCharCode -= 26;
      }

      // Convert the shifted ASCII code back to a character.
      const shiftedChar = String.fromCharCode(shiftedCharCode);

      // Append the shifted character to the encrypted message.
      encrypted += shiftedChar;
    } else {
      // If the current character is not an alphabet, leave it as it is.
      encrypted += string[i];
    }
  }

  // Finally, return the encrypted message.
  return encrypted;
}

// This code defines a function called 'mergeSort' that sorts an array of numbers using the merge sort algorithm.
function mergeSort(array) {
  // If the array has only one element, it is already sorted, so return it as is.
  if (array.length <= 1) {
    return array;
  }

  // Find the midpoint of the array.
  const mid = Math.floor(array.length / 2);

  // Split the array into two halves.
  const leftHalf = array.slice(0, mid);
  const rightHalf = array.slice(mid);

  // Recursively sort the left and right halves.
  const sortedLeft = mergeSort(leftHalf);
  const sortedRight = mergeSort(rightHalf);

  // Merge the two sorted halves into one sorted array.
  const mergedArray = [];
  let leftIndex = 0;
  let rightIndex = 0;

  while (leftIndex < sortedLeft.length && rightIndex < sortedRight.length) {
    if (sortedLeft[leftIndex] < sortedRight[rightIndex]) {
      mergedArray.push(sortedLeft[leftIndex]);
      leftIndex++;
    } else {
      mergedArray.push(sortedRight[rightIndex]);
      rightIndex++;
    }
  }

  // Append the remaining elements from either half if any.
  while (leftIndex < sortedLeft.length) {
    mergedArray.push(sortedLeft[leftIndex]);
    leftIndex++;
  }
  while (rightIndex < sortedRight.length) {
    mergedArray.push(sortedRight[rightIndex]);
    rightIndex++;
  }

  // Finally, return the merged and sorted array.
  return mergedArray;
}

// This code defines a function called 'dijkstra' that finds the shortest paths from a source node to all other nodes in a weighted graph.
function dijkstra(graph, sourceNode) {
  // Initialize distances to all nodes as infinity, except the source node which is set to 0.
  const distances = {};
  for (const node in graph) {
    distances[node] = Infinity;
  }
  distances[sourceNode] = 0;

  // Initialize the unvisited nodes.
  const unvisitedNodes = Object.keys(graph);

  // While there are still unvisited nodes, continue the algorithm.
  while (unvisitedNodes.length > 0) {
    // Find the unvisited node with the smallest distance.
    let currentNode = null;
    let minDistance = Infinity;
    for (const node of unvisitedNodes) {
      if (distances[node] < minDistance) {
        currentNode = node;
        minDistance = distances[node];
      }
    }

    // If all distances are infinity, no path exists, so break.
    if (minDistance === Infinity) {
      break;
    }

    // Mark the current node as visited.
    unvisitedNodes.splice(unvisitedNodes.indexOf(currentNode), 1);

    // Update the distances to the current node's neighbors.
    const neighbors = graph[currentNode];
    for (const neighbor in neighbors) {
      const distanceToNeighbor = distances[currentNode] + neighbors[neighbor];
      if (distanceToNeighbor < distances[neighbor]) {
        distances[neighbor] = distanceToNeighbor;
      }
    }
  }

  // Return the distances from the source node to all other nodes.
  return distances;
}
```

**Explanation**:

1. `factorial`: Calculates the factorial of a number using a loop.
2. `isPrime`: Checks if a given number is prime using a loop.
3. `fibonacci`: Generates the Fibonacci sequence up to a given number.
4. `caesarCipher`: Encrypts a string using the Caesar cipher.
5. `mergeSort`: Sorts an array of numbers using the merge sort algorithm.
6. `dijkstra`: Finds the shortest paths from a source node to all other nodes in a weighted graph using Dijkstra's algorithm.