```php
<?php
// Function to calculate the factorial of a number
function factorial($n) {
  if ($n === 0) {
    return 1;
  } else {
    return $n * factorial($n - 1);
  }
}

// Function to check if a number is prime
function is_prime($n) {
  if ($n <= 1) {
    return false;
  }
  for ($i = 2; $i <= sqrt($n); $i++) {
    if ($n % $i === 0) {
      return false;
    }
  }
  return true;
}

// Function to generate a random string of a given length
function generate_random_string($length) {
  $characters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789';
  $string = '';
  for ($i = 0; $i < $length; $i++) {
    $string .= $characters[rand(0, strlen($characters) - 1)];
  }
  return $string;
}

// Function to sort an array of associative arrays by a given key
function sort_by_key($array, $key) {
  usort($array, function($a, $b) use ($key) {
    return strcmp($a[$key], $b[$key]);
  });
}

// Function to find the intersection of two arrays
function array_intersection($array1, $array2) {
  $intersection = array();
  foreach ($array1 as $value) {
    if (in_array($value, $array2)) {
      $intersection[] = $value;
    }
  }
  return $intersection;
}

// Function to find the union of two arrays
function array_union($array1, $array2) {
  $union = array();
  foreach ($array1 as $value) {
    if (!in_array($value, $union)) {
      $union[] = $value;
    }
  }
  foreach ($array2 as $value) {
    if (!in_array($value, $union)) {
      $union[] = $value;
    }
  }
  return $union;
}

// Function to find the difference between two arrays
function array_difference($array1, $array2) {
  $difference = array();
  foreach ($array1 as $value) {
    if (!in_array($value, $array2)) {
      $difference[] = $value;
    }
  }
  return $difference;
}

// Function to convert a multidimensional array to a single-dimensional array
function array_flatten($array) {
  $flattened = array();
  foreach ($array as $value) {
    if (is_array($value)) {
      $flattened = array_merge($flattened, array_flatten($value));
    } else {
      $flattened[] = $value;
    }
  }
  return $flattened;
}

// Function to shuffle an array
function array_shuffle($array) {
  shuffle($array);
  return $array;
}

// Function to reverse an array
function array_reverse($array) {
  $reversed = array();
  foreach ($array as $value) {
    $reversed[] = $value;
  }
  return $reversed;
}

// Function to search for a value in an array
function array_search($value, $array) {
  foreach ($array as $key => $val) {
    if ($val === $value) {
      return $key;
    }
  }
  return false;
}

// Function to remove a value from an array
function array_remove($value, $array) {
  $index = array_search($value, $array);
  if ($index !== false) {
    unset($array[$index]);
  }
  return $array;
}

// Function to insert a value into an array at a specific index
function array_insert($value, $array, $index) {
  $array = array_slice($array, 0, $index);
  $array[] = $value;
  $array = array_merge($array, array_slice($array, $index));
  return $array;
}

?>
```

This code contains a collection of useful functions that can be used in PHP development. The functions include:

* `factorial`: Calculates the factorial of a number.
* `is_prime`: Checks if a number is prime.
* `generate_random_string`: Generates a random string of a given length.
* `sort_by_key`: Sorts an array of associative arrays by a given key.
* `array_intersection`: Finds the intersection of two arrays.
* `array_union`: Finds the union of two arrays.
* `array_difference`: Finds the difference between two arrays.
* `array_flatten`: Converts a multidimensional array to a single-dimensional array.
* `array_shuffle`: Shuffles an array.
* `array_reverse`: Reverses an array.
* `array_search`: Searches for a value in an array.
* `array_remove`: Removes a value from an array.
* `array_insert`: Inserts a value into an array at a specific index.

These functions can be used in a variety of PHP scripts to perform common tasks such as generating random data, sorting data, and finding values in arrays.