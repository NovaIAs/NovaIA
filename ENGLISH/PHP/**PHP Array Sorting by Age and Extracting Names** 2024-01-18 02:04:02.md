```php
// Define a multidimensional array of names and ages
$people = [
    ['name' => 'John Doe', 'age' => 25],
    ['name' => 'Jane Smith', 'age' => 30],
    ['name' => 'Michael Jones', 'age' => 35],
    ['name' => 'Mary Johnson', 'age' => 40],
    ['name' => 'Robert Brown', 'age' => 45],
];

// Define a function to sort the array by age
function sortByAge($a, $b) {
    return $a['age'] - $b['age'];
}

// Sort the array using the sortByAge function
usort($people, 'sortByAge');

// Create a new array to store the sorted names
$sortedNames = array_column($people, 'name');

// Print the sorted names
echo implode(', ', $sortedNames);
```

**Explanation:**

1. We define a multidimensional array `$people` that contains information about names and ages of different individuals. Each element in the array represents a person and has two keys: `name` and `age`.


2. We define a function `sortByAge` that takes two arrays as arguments and compares their `age` values. It returns a negative value if the first array's age is less than the second array's age, a positive value if the first array's age is greater than the second array's age, and zero if both ages are equal.


3. We use the `usort` function to sort the `$people` array using the `sortByAge` function as a comparison function. This sorts the array in ascending order of age.


4. We use the `array_column` function to extract the `name` values from the sorted `$people` array and store them in a new array `$sortedNames`.


5. Finally, we use the `implode` function to concatenate the elements of the `$sortedNames` array into a single string and print it.

This code demonstrates a complex and differentiated PHP code that performs the following tasks:

- Defines a multidimensional array containing information about names and ages.
- Defines a function to sort the array by age in ascending order.
- Sorts the array using the defined function.
- Extracts the names from the sorted array and stores them in a new array.
- Concatenates the names into a single string and prints it.

The code is well-structured, organized, and efficient, making it a good example of complex PHP code.