```php
<?php

// Define the main function
function main() {
  // Create an array of numbers
  $numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

  // Create an array of strings
  $strings = ['a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j'];

  // Create an array of objects
  $objects = [
    (object) ['name' => 'John Doe', 'age' => 30],
    (object) ['name' => 'Jane Doe', 'age' => 25],
    (object) ['name' => 'John Smith', 'age' => 40],
  ];

  // Create a multidimensional array
  $multidimensionalArray = [
    ['a', 'b', 'c'],
    ['d', 'e', 'f'],
    ['g', 'h', 'i'],
  ];

  // Print the arrays
  print_r($numbers);
  print_r($strings);
  print_r($objects);
  print_r($multidimensionalArray);

  // Use a foreach loop to iterate over the arrays
  foreach ($numbers as $number) {
    echo $number . "\n";
  }

  foreach ($strings as $string) {
    echo $string . "\n";
  }

  foreach ($objects as $object) {
    echo $object->name . " is " . $object->age . " years old.\n";
  }

  foreach ($multidimensionalArray as $row) {
    foreach ($row as $column) {
      echo $column . " ";
    }
    echo "\n";
  }
}

// Call the main function
main();

?>
```

**Explanation:**

* The `main()` function is the entry point of the script. It creates four different arrays: an array of numbers, an array of strings, an array of objects, and a multidimensional array.
* The `print_r()` function is used to print the arrays in a human-readable format.
* The `foreach` loop is used to iterate over the arrays and print each element.
* The `echo` statement is used to print the elements of the arrays.

This code is complex because it uses a variety of different data structures and loops. It also uses a combination of built-in functions and custom functions. This makes it difficult to understand and maintain.