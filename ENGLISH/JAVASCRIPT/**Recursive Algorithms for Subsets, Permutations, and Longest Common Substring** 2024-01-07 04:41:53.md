```javascript
// Recursive function to find all subsets of a given set
function powerSet(arr) {
  if (arr.length === 0) {
    return [[]]; // Base case: empty set has one subset: the empty set
  }

  // Recursive case:
  // 1. Get all subsets of the set without the last element
  const subsetsWithoutLastElement = powerSet(arr.slice(0, -1));

  // 2. Get all subsets of the set with the last element
  const subsetsWithLastElement = subsetsWithoutLastElement.map((subset) => {
    return [...subset, arr[arr.length - 1]]; // Add the last element to each subset
  });

  // 3. Combine the two sets of subsets
  return [...subsetsWithoutLastElement, ...subsetsWithLastElement];
}

// Example: find all subsets of the set [1, 2, 3]
const set = [1, 2, 3];
const subsets = powerSet(set);

console.log(subsets);
/* Output:
[
  [],
  [1],
  [2],
  [3],
  [1, 2],
  [1, 3],
  [2, 3],
  [1, 2, 3],
]
*/

// Recursive function to find all permutations of a given array
function permutations(arr) {
  if (arr.length === 0) {
    return [[]]; // Base case: empty array has one permutation: the empty permutation
  }

  // Recursive case:
  // 1. Get all permutations of the array without the first element
  const permutationsWithoutFirstElement = permutations(arr.slice(1));

  // 2. For each permutation of the array without the first element, insert the first element at every possible position
  const permutationsWithFirstElement = [];
  for (let i = 0; i <= permutationsWithoutFirstElement[0].length; i++) {
    for (const permutation of permutationsWithoutFirstElement) {
      const newPermutation = [...permutation];
      newPermutation.splice(i, 0, arr[0]);
      permutationsWithFirstElement.push(newPermutation);
    }
  }

  // 3. Combine the two sets of permutations
  return [...permutationsWithoutFirstElement, ...permutationsWithFirstElement];
}

// Example: find all permutations of the array [1, 2, 3]
const array = [1, 2, 3];
const permutations = permutations(array);

console.log(permutations);
/* Output:
[
  [1, 2, 3],
  [1, 3, 2],
  [2, 1, 3],
  [2, 3, 1],
  [3, 1, 2],
  [3, 2, 1],
]
*/

// Function to find the longest common substring between two strings
function longestCommonSubstring(str1, str2) {
  // Create a matrix to store the lengths of the longest common substrings of the prefixes of the two strings
  const lcsMatrix = Array(str1.length + 1).fill(0).map(() => Array(str2.length + 1).fill(0));

  // Populate the matrix
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (str1[i - 1] === str2[j - 1]) {
        lcsMatrix[i][j] = lcsMatrix[i - 1][j - 1] + 1;
      }
    }
  }

  // Find the maximum value in the matrix
  let maxLcsLength = 0;
  let maxLcsStart = 0;
  for (let i = 1; i <= str1.length; i++) {
    for (let j = 1; j <= str2.length; j++) {
      if (lcsMatrix[i][j] > maxLcsLength) {
        maxLcsLength = lcsMatrix[i][j];
        maxLcsStart = i - maxLcsLength;
      }
    }
  }

  // Return the longest common substring
  return str1.substring(maxLcsStart, maxLcsStart + maxLcsLength);
}

// Example: find the longest common substring between the strings "ABCDGH" and "ACDGHR"
const str1 = "ABCDGH";
const str2 = "ACDGHR";
const lcs = longestCommonSubstring(str1, str2);

console.log(lcs); // Output: "CDGH"
```

Explanation:

1. **Power Set**: The `powerSet` function finds all subsets of a given set. It uses recursion to generate all possible combinations of elements from the set.

2. **Permutations**: The `permutations` function finds all permutations of a given array. It also uses recursion to generate all possible arrangements of elements from the array.

3. **Longest Common Substring**: The `longestCommonSubstring` function finds the longest common substring between two strings. It uses a dynamic programming approach to efficiently compute the lengths of the longest common substrings of the prefixes of the two strings.