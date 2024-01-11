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
  } elseif ($n <= 3) {
    return true;
  } elseif ($n % 2 == 0 || $n % 3 == 0) {
    return false;
  } else {
    for ($i = 5; $i * $i <= $n; $i += 6) {
      if ($n % $i == 0 || $n % ($i + 2) == 0) {
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

// Define a function to check if a string is a palindrome
function is_palindrome($str) {
  $str = strtolower($str);
  $str = preg_replace('/[^a-zA-Z0-9]/', '', $str);
  return $str == strrev($str);
}

// Define a function to reverse a string
function reverse_string($str) {
  return strrev($str);
}

// Define a function to sort an array in ascending order
function sort_ascending($arr) {
  sort($arr);
  return $arr;
}

// Define a function to sort an array in descending order
function sort_descending($arr) {
  rsort($arr);
  return $arr;
}

// Define a function to shuffle an array
function shuffle_array($arr) {
  shuffle($arr);
  return $arr;
}

// Define a function to find the maximum value in an array
function max_value($arr) {
  return max($arr);
}

// Define a function to find the minimum value in an array
function min_value($arr) {
  return min($arr);
}

// Define a function to find the average value in an array
function average_value($arr) {
  return array_sum($arr) / count($arr);
}

// Define a function to find the median value in an array
function median_value($arr) {
  sort($arr);
  $middle = floor(count($arr) / 2);
  if (count($arr) % 2 == 0) {
    return ($arr[$middle] + $arr[$middle - 1]) / 2;
  } else {
    return $arr[$middle];
  }
}

// Define a function to find the mode value in an array
function mode_value($arr) {
  $values = array_count_values($arr);
  $max_count = max($values);
  $modes = array_keys($values, $max_count);
  return $modes;
}

// Define a function to find the range of values in an array
function range_value($arr) {
  sort($arr);
  return $arr[count($arr) - 1] - $arr[0];
}

// Define a function to find the variance of values in an array
function variance_value($arr) {
  $mean = average_value($arr);
  $deviations = array_map(function ($x) use ($mean) {
    return pow($x - $mean, 2);
  }, $arr);
  return array_sum($deviations) / count($arr);
}

// Define a function to find the standard deviation of values in an array
function standard_deviation_value($arr) {
  return sqrt(variance_value($arr));
}

?>
```

This code is a collection of various mathematical and string manipulation functions in PHP. It includes functions for calculating factorials, Fibonacci numbers, checking primality, finding the greatest common divisor and least common multiple, checking for palindromes, reversing strings, sorting arrays, finding maximum, minimum, average, median, mode, range, variance, and standard deviation. The functions are all well-commented and easy to understand.