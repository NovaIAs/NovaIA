```javascript
// Recursive function to calculate the factorial of a number using tail call optimization
const factorial = (n, acc = 1) => {
  if (n === 0) {
    return acc;
  } else {
    return factorial(n - 1, acc * n);
  }
};

// Function to generate a random number between a minimum and maximum value
const getRandomNumber = (min, max) => {
  return Math.random() * (max - min) + min;
};

// Function to shuffle an array of elements using Fisher-Yates algorithm
const shuffleArray = (array) => {
  for (let i = array.length - 1; i > 0; i--) {
    const j = Math.floor(Math.random() * (i + 1));
    [array[i], array[j]] = [array[j], array[i]];
  }
  return array;
};

// Function to sort an array of numbers in ascending order using the quicksort algorithm
const quickSort = (array) => {
  if (array.length <= 1) {
    return array;
  }
  const pivot = array[0];
  const left = [];
  const right = [];
  for (let i = 1; i < array.length; i++) {
    if (array[i] < pivot) {
      left.push(array[i]);
    } else {
      right.push(array[i]);
    }
  }
  return [...quickSort(left), pivot, ...quickSort(right)];
};

// Function to create a binary search tree from an array of numbers
const createBinarySearchTree = (array) => {
  if (array.length === 0) {
    return null;
  }
  const mid = Math.floor(array.length / 2);
  const root = new TreeNode(array[mid]);
  root.left = createBinarySearchTree(array.slice(0, mid));
  root.right = createBinarySearchTree(array.slice(mid + 1));
  return root;
};

// Function to check if a binary tree is a binary search tree
const isBinarySearchTree = (root) => {
  if (root === null) {
    return true;
  }
  const leftValid = isBinarySearchTree(root.left);
  const rightValid = isBinarySearchTree(root.right);
  if (!leftValid || !rightValid) {
    return false;
  }
  if (root.left !== null && root.left.val >= root.val) {
    return false;
  }
  if (root.right !== null && root.right.val <= root.val) {
    return false;
  }
  return true;
};

// Function to convert a binary tree to an array of its values in level order traversal
const levelOrderTraversal = (root) => {
  if (root === null) {
    return [];
  }
  const queue = [root];
  const result = [];
  while (queue.length > 0) {
    const node = queue.shift();
    result.push(node.val);
    if (node.left !== null) {
      queue.push(node.left);
    }
    if (node.right !== null) {
      queue.push(node.right);
    }
  }
  return result;
};

// Function to find the shortest path between two nodes in a graph represented as an adjacency list
const shortestPath = (graph, start, end) => {
  const queue = [{ node: start, distance: 0 }];
  const visited = new Set();
  while (queue.length > 0) {
    const { node, distance } = queue.shift();
    if (node === end) {
      return distance;
    }
    if (!visited.has(node)) {
      visited.add(node);
      for (const neighbor of graph[node]) {
        queue.push({ node: neighbor, distance: distance + 1 });
      }
    }
  }
  return -1;
};

// Function to find the maximum sum of a contiguous subarray in an array of numbers
const maxSubarraySum = (array) => {
  let maxSoFar = 0;
  let maxEndingHere = 0;
  for (let i = 0; i < array.length; i++) {
    maxEndingHere = maxEndingHere + array[i];
    if (maxSoFar < maxEndingHere) {
      maxSoFar = maxEndingHere;
    }
    if (maxEndingHere < 0) {
      maxEndingHere = 0;
    }
  }
  return maxSoFar;
};

// Function to find the longest common subsequence between two strings
const longestCommonSubsequence = (s1, s2) => {
  const dp = new Array(s1.length + 1).fill(0).map(() => new Array(s2.length + 1).fill(0));
  for (let i = 1; i <= s1.length; i++) {
    for (let j = 1; j <= s2.length; j++) {
      if (s1[i - 1] === s2[j - 1]) {
        dp[i][j] = dp[i - 1][j - 1] + 1;
      } else {
        dp[i][j] = Math.max(dp[i - 1][j], dp[i][j - 1]);
      }
    }
  }
  let lcs = "";
  let i = s1.length;
  let j = s2.length;
  while (i > 0 && j > 0) {
    if (s1[i - 1] === s2[j - 1]) {
      lcs = s1[i - 1] + lcs;
      i--;
      j--;
    } else if (dp[i - 1][j] > dp[i][j - 1]) {
      i--;
    } else {
      j--;
    }
  }
  return lcs;
};

// Function to implement the knapsack problem using dynamic programming
const knapsack = (items, capacity) => {
  const dp = new Array(items.length + 1).fill(0).map(() => new Array(capacity + 1).fill(0));
  for (let i = 1; i <= items.length; i++) {
    for (let j = 1; j <= capacity; j++) {
      if (items[i - 1].weight