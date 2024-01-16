```javascript
// Recursive function to generate all possible combinations of a given array of elements
function getAllCombinations(elements) {
  if (elements.length === 0) {
    return [[]];
  }

  const firstElement = elements[0];
  const remainingElements = elements.slice(1);
  const combinationsWithoutFirstElement = getAllCombinations(remainingElements);

  const combinationsWithFirstElement = combinationsWithoutFirstElement.map(combination => [
    firstElement,
    ...combination,
  ]);

  return [...combinationsWithoutFirstElement, ...combinationsWithFirstElement];
}

// Example usage
const elements = [1, 2, 3, 4];
const combinations = getAllCombinations(elements);
console.log(combinations);
```

Explanation:

1. The getAllCombinations function takes an array of elements as input and returns an array of all possible combinations of those elements.
2. The function uses recursion to generate all possible combinations. The base case of the recursion is when the input array is empty, in which case the function returns an array containing an empty array.
3. For each element in the input array, the function generates all possible combinations of the remaining elements in the array, and then adds the current element to the beginning of each of those combinations.
4. The function returns the combination of arrays without the first element and the combinations of arrays with the first element.
5. The example usage shows how to use the getAllCombinations function to generate all possible combinations of the array [1, 2, 3, 4]. The output of the function is an array containing 16 arrays, each representing a different combination of the elements in the input array.