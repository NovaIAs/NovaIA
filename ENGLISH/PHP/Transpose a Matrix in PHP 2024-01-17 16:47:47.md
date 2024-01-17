```php
// Define the input array
$inputArray = array(
    array(1, 2, 3),
    array(4, 5, 6),
    array(7, 8, 9)
);

// Create an empty array to store the transposed matrix
$transposedArray = array();

// Iterate over the input array to build the transposed matrix
for ($i = 0; $i < count($inputArray[0]); $i++) {
    $transposedArray[$i] = array();
    for ($j = 0; $j < count($inputArray); $j++) {
        $transposedArray[$i][$j] = $inputArray[$j][$i];
    }
}

// Print the original array
echo "Original Array:<br>";
for ($i = 0; $i < count($inputArray); $i++) {
    for ($j = 0; $j < count($inputArray[0]); $j++) {
        echo $inputArray[$i][$j] . " ";
    }
    echo "<br>";
}

// Print the transposed array
echo "<br>Transposed Array:<br>";
for ($i = 0; $i < count($transposedArray); $i++) {
    for ($j = 0; $j < count($transposedArray[0]); $j++) {
        echo $transposedArray[$i][$j] . " ";
    }
    echo "<br>";
}
```

**Explanation:**

1. **Input Array:** `$inputArray` is defined as a multidimensional array containing three rows and three columns. Each element represents a value in the matrix.

2. **Create Transposed Matrix:** An empty array `$transposedArray` is created to store the transposed matrix.

3. **Transpose the Matrix:** The outer loop (`for ($i = 0; $i < count($inputArray[0]); $i++)`) iterates over the number of columns in the input array. The inner loop (`for ($j = 0; $j < count($inputArray); $j++)`) iterates over the number of rows in the input array. During each iteration, the element at position `$inputArray[$j][$i]` is copied to `$transposedArray[$i][$j]`. This effectively transposes the input matrix.

4. **Print the Original Array:** The original array `$inputArray` is printed using nested loops to display each element in a grid format.

5. **Print the Transposed Array:** The transposed array `$transposedArray` is printed using nested loops to display each element in a grid format.

This code demonstrates the transposition of a matrix, which involves swapping the rows and columns of the original matrix to create a new matrix. The result is a new matrix with the same elements but a different arrangement.