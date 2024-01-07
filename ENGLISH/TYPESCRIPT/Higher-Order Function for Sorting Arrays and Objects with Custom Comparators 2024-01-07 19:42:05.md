```typescript
// Create a Higher-Order Function that takes a comparator function and returns a sorting function.
const createSortFunction = (comparator: (a: number, b: number) => number) => {
  return (array: number[]): number[] => {
    const sortedArray = [...array];  // Create a copy of the array to avoid mutating the original.
    sortedArray.sort(comparator);  // Use the comparator function to sort the copy of the array.
    return sortedArray;  // Return the sorted array.
  };
};

// Create a comparator function for ascending order.
const ascendingComparator = (a: number, b: number): number => {
  return a - b;  // Subtract b from a to get the difference between them.
};

// Create a comparator function for descending order.
const descendingComparator = (a: number, b: number): number => {
  return b - a;  // Subtract a from b to get the difference between them.
};

// Create a comparator function for case-insensitive string comparison.
const caseInsensitiveStringComparator = (a: string, b: string): number => {
  return a.toLowerCase().localeCompare(b.toLowerCase());  // Compare the lowercase versions of the strings using localeCompare().
};

// Create a comparator function for sorting objects by a specific property.
const sortByPropertyComparator = (propertyName: string) => {
  return (a: { [key: string]: any }, b: { [key: string]: any }): number => {
    return a[propertyName] - b[propertyName];  // Subtract the values of the specified property from each object.
  };
};

// Create an array of numbers.
const numbers = [5, 2, 9, 1, 3, 8, 4, 7, 6];

// Sort the array in ascending order using the createSortFunction() function.
const sortedAscending = createSortFunction(ascendingComparator)(numbers);

// Sort the array in descending order using the createSortFunction() function.
const sortedDescending = createSortFunction(descendingComparator)(numbers);

// Sort the array of numbers in case-insensitive ascending order using the createSortFunction() function.
const numbersCaseInsensitiveAscending = createSortFunction((a: string, b: string) => a.toLowerCase().localeCompare(b.toLowerCase()))([
  "Apple",
  "Banana",
  "Cherry",
  "Date",
  "Elderberry",
  "Fig",
]);

// Sort an array of objects by a specific property using the createSortFunction() function.
const objects = [
  { name: "John", age: 30 },
  { name: "Jane", age: 25 },
  { name: "Bob", age: 35 },
  { name: "Alice", age: 28 },
];
const sortedObjectsByAge = createSortFunction(sortByPropertyComparator("age"))(objects);

// Print the sorted arrays and objects.
console.log("Sorted in ascending order:", sortedAscending);
console.log("Sorted in descending order:", sortedDescending);
console.log("Sorted case-insensitive ascending order:", numbersCaseInsensitiveAscending);
console.log("Sorted objects by age:", sortedObjectsByAge);
```

Explanation:

1. **Higher-Order Function createSortFunction():** This function takes a comparator function as an argument and returns a sorting function. The sorting function can then be used to sort an array of elements using the provided comparator function.

2. **Comparator Functions:**
   - **ascendingComparator:** This comparator function is used to sort numbers in ascending order. It subtracts the second number (b) from the first number (a) to get the difference. If the difference is positive, it means that a is greater than b and should come after b in the sorted order. If the difference is negative, it means that a is less than b and should come before b in the sorted order. If the difference is zero, it means that a and b are equal and their order does not matter.
   - **descendingComparator:** This comparator function is used to sort numbers in descending order. It subtracts the first number (a) from the second number (b) to get the difference. If the difference is positive, it means that a is greater than b and should come before b in the sorted order. If the difference is negative, it means that a is less than b and should come after b in the sorted order. If the difference is zero, it means that a and b are equal and their order does not matter.
   - **caseInsensitiveStringComparator:** This comparator function is used to sort strings in case-insensitive ascending order. It converts both strings to lowercase using toLowerCase() and then compares them using localeCompare(). If the result of localeCompare() is negative, it means that a comes before b in the sorted order. If the result is positive, it means that a comes after b in the sorted order. If the result is zero, it means that a and b are equal and their order does not matter.
   - **sortByPropertyComparator:** This comparator function is used to sort objects by a specific property. It takes the property name as an argument and returns a comparator function that compares the values of that property in two objects. If the difference between the property values is positive, it means that the first object should come after the second object in the sorted order. If the difference is negative, it means that the first object should come before the second object in the sorted order. If the difference is zero, it means that the property values are equal and the order of the objects does not matter.

3. **Usage of createSortFunction():** The createSortFunction() function is used to create sorting functions for different scenarios. For example, we can create a sorting function for sorting numbers in ascending order by passing the ascendingComparator function as an argument. Similarly, we can create sorting functions for sorting numbers in descending order, sorting strings in case-insensitive ascending order, and sorting objects by a specific property.

4. **Sorting Arrays and Objects:** We create an array of numbers and an array of objects. We then use the createSortFunction() function to create sorting functions for different scenarios and use these functions to sort the arrays and objects.

5. **Printing the Results:** Finally, we print the sorted arrays and objects to the console to see the results.