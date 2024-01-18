```php
<?php
// Define the input array
$input = array(
    array('name' => 'John Doe', 'age' => 25, 'city' => 'New York'),
    array('name' => 'Jane Smith', 'age' => 30, 'city' => 'London'),
    array('name' => 'Michael Jones', 'age' => 35, 'city' => 'Paris'),
    array('name' => 'Sarah Miller', 'age' => 40, 'city' => 'Rome'),
    array('name' => 'Robert Brown', 'age' => 45, 'city' => 'Berlin')
);

// Function to filter the input array based on a given condition
function filterArray($input, $condition) {
    $filteredArray = array_filter($input, function($element) use ($condition) {
        return $element[$condition] == true;
    });
    return $filteredArray;
}

// Function to sort the input array based on a given key
function sortArray($input, $key) {
    usort($input, function($a, $b) use ($key) {
        return strcmp($a[$key], $b[$key]);
    });
    return $input;
}

// Get the input array and filter it based on the condition 'age > 30'
$filteredArray = filterArray($input, 'age > 30');

// Sort the filtered array based on the key 'name'
$sortedArray = sortArray($filteredArray, 'name');

// Print the sorted array
echo "<pre>";
print_r($sortedArray);
echo "</pre>";

?>
```

Explanation:

1. **Input Array**: We define an input array `$input` containing information about individuals, including their name, age, and city.

2. **`filterArray()` Function**: We define a function `filterArray()` that takes an array `$input` and a condition `$condition` as arguments. This function uses the built-in PHP function `array_filter()`, which filters the input array based on the specified condition.

3. **Filtering the Array**: We call the `filterArray()` function with the `$input` array and the condition `'age > 30'`. This filters the input array to include only individuals over 30 years old. The result is stored in the `$filteredArray` variable.

4. **`sortArray()` Function**: We define a function `sortArray()` that takes an array `$input` and a key `$key` as arguments. This function uses the built-in PHP function `usort()`, which sorts the input array based on the specified key.

5. **Sorting the Filtered Array**: We call the `sortArray()` function with the `$filteredArray` and the key `'name'`. This sorts the filtered array in ascending order based on the individuals' names. The sorted array is stored in the `$sortedArray` variable.

6. **Printing the Sorted Array**: Finally, we use `echo` and `print_r()` to print the sorted array in a readable format.