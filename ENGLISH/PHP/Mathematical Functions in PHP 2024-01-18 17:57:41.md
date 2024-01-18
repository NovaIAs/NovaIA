```php
<?php

// Define a function to calculate the factorial of a number
function factorial($n) {
  if ($n == 0) {
    return 1;
  } else {
    return $n * factorial($n - 1);
  }
}

// Define a function to calculate the Fibonacci sequence
function fibonacci($n) {
  if ($n == 0) {
    return 0;
  } elseif ($n == 1) {
    return 1;
  } else {
    return fibonacci($n - 1) + fibonacci($n - 2);
  }
}

// Define a function to check if a number is prime
function is_prime($n) {
  if ($n <= 1) {
    return false;
  } else {
    for ($i = 2; $i <= sqrt($n); $i++) {
      if ($n % $i == 0) {
        return false;
      }
    }
    return true;
  }
}

// Define a function to find the greatest common divisor of two numbers
function gcd($a, $b) {
  if ($b == 0) {
    return $a;
  } else {
    return gcd($b, $a % $b);
  }
}

// Define a function to find the least common multiple of two numbers
function lcm($a, $b) {
  return ($a * $b) / gcd($a, $b);
}

// Define a function to generate a random number between two numbers
function rand_range($min, $max) {
  return rand($min, $max - 1) + 1;
}

// Define a function to shuffle an array
function shuffle_array($array) {
  for ($i = 0; $i < count($array); $i++) {
    $j = rand_range(0, count($array));
    $temp = $array[$i];
    $array[$i] = $array[$j];
    $array[$j] = $temp;
  }
  return $array;
}

// Define a function to sort an array in ascending order
function sort_ascending($array) {
  sort($array);
  return $array;
}

// Define a function to sort an array in descending order
function sort_descending($array) {
  rsort($array);
  return $array;
}

// Define a function to find the maximum value in an array
function max_value($array) {
  return max($array);
}

// Define a function to find the minimum value in an array
function min_value($array) {
  return min($array);
}

// Define a function to find the sum of an array
function sum_array($array) {
  return array_sum($array);
}

// Define a function to find the average value of an array
function average_value($array) {
  return sum_array($array) / count($array);
}

// Define a function to find the median value of an array
function median_value($array) {
  sort_ascending($array);
  $middle = count($array) / 2;
  if (count($array) % 2 == 0) {
    return ($array[$middle] + $array[$middle - 1]) / 2;
  } else {
    return $array[$middle];
  }
}

// Define a function to find the mode value of an array
function mode_value($array) {
  $values = array_count_values($array);
  $max_value = max($values);
  $modes = array();
  foreach ($values as $value => $count) {
    if ($count == $max_value) {
      $modes[] = $value;
    }
  }
  return $modes;
}

// Define a function to find the range of an array
function range_value($array) {
  sort_ascending($array);
  return max_value($array) - min_value($array);
}

// Define a function to find the variance of an array
function variance_value($array) {
  $mean = average_value($array);
  $sum_of_squares = 0;
  foreach ($array as $value) {
    $sum_of_squares += pow($value - $mean, 2);
  }
  return $sum_of_squares / (count($array) - 1);
}

// Define a function to find the standard deviation of an array
function standard_deviation_value($array) {
  return sqrt(variance_value($array));
}

?>
```

This code is a collection of mathematical functions written in PHP. It includes functions to calculate the factorial of a number, the Fibonacci sequence, check if a number is prime, find the greatest common divisor and least common multiple of two numbers, generate a random number between two numbers, shuffle an array, sort an array in ascending or descending order, find the maximum, minimum, sum, average, median, mode, range, variance, and standard deviation of an array. These functions can be used for various mathematical operations and statistical analysis.