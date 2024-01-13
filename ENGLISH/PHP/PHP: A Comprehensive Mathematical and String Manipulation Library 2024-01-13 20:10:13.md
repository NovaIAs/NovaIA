```php
<?php

// Define a function to calculate the factorial of a number
function factorial($number) {
  if ($number < 0) {
    throw new Exception("Factorial is undefined for negative numbers");
  }
  if ($number == 0) {
    return 1;
  }
  $factorial = 1;
  for ($i = 1; $i <= $number; $i++) {
    $factorial *= $i;
  }
  return $factorial;
}

// Define a function to calculate the Fibonacci sequence
function fibonacci($number) {
  if ($number < 0) {
    throw new Exception("Fibonacci sequence is undefined for negative numbers");
  }
  if ($number == 0 || $number == 1) {
    return $number;
  }
  $fibonacciSequence = array(0, 1);
  for ($i = 2; $i <= $number; $i++) {
    $fibonacciSequence[] = $fibonacciSequence[$i - 1] + $fibonacciSequence[$i - 2];
  }
  return $fibonacciSequence[$number];
}

// Define a function to check if a number is prime
function isPrime($number) {
  if ($number <= 1) {
    return false;
  }
  for ($i = 2; $i <= sqrt($number); $i++) {
    if ($number % $i == 0) {
      return false;
    }
  }
  return true;
}

// Define a function to find the greatest common divisor of two numbers
function gcd($number1, $number2) {
  while ($number2 != 0) {
    $temp = $number2;
    $number2 = $number1 % $number2;
    $number1 = $temp;
  }
  return $number1;
}

// Define a function to find the least common multiple of two numbers
function lcm($number1, $number2) {
  return ($number1 * $number2) / gcd($number1, $number2);
}

// Define a function to convert a number from decimal to binary
function dec2bin($number) {
  if ($number == 0) {
    return "0";
  }
  $binaryNumber = "";
  while ($number > 0) {
    $binaryNumber = ($number % 2) . $binaryNumber;
    $number = floor($number / 2);
  }
  return $binaryNumber;
}

// Define a function to convert a number from binary to decimal
function bin2dec($binaryNumber) {
  $decimalNumber = 0;
  $power = 1;
  for ($i = strlen($binaryNumber) - 1; $i >= 0; $i--) {
    $decimalNumber += (int)$binaryNumber[$i] * $power;
    $power *= 2;
  }
  return $decimalNumber;
}

// Define a function to check if a string is a palindrome
function isPalindrome($string) {
  $string = strtolower($string);
  $string = preg_replace('/[^a-zA-Z0-9]/', '', $string);
  $reversedString = strrev($string);
  return $string == $reversedString;
}

// Define a function to reverse a string
function reverseString($string) {
  $reversedString = "";
  for ($i = strlen($string) - 1; $i >= 0; $i--) {
    $reversedString .= $string[$i];
  }
  return $reversedString;
}

// Define a function to shuffle an array
function shuffleArray($array) {
  shuffle($array);
  return $array;
}

// Define a function to sort an array
function sortArray($array) {
  sort($array);
  return $array;
}

// Define a function to search for a value in an array
function searchArray($array, $value) {
  return array_search($value, $array);
}

// Define a function to find the maximum value in an array
function maxArray($array) {
  return max($array);
}

// Define a function to find the minimum value in an array
function minArray($array) {
  return min($array);
}

// Define a function to sum the values in an array
function sumArray($array) {
  return array_sum($array);
}

// Define a function to calculate the average of the values in an array
function averageArray($array) {
  return array_sum($array) / count($array);
}

// Define a function to find the median of the values in an array
function medianArray($array) {
  sort($array);
  $middleIndex = floor(count($array) / 2);
  if (count($array) % 2 == 0) {
    return ($array[$middleIndex] + $array[$middleIndex - 1]) / 2;
  } else {
    return $array[$middleIndex];
  }
}

// Define a function to find the mode of the values in an array
function modeArray($array) {
  $modes = array();
  $maxCount = 0;
  foreach ($array as $value) {
    $count = array_count_values($array)[$value];
    if ($count > $maxCount) {
      $modes = array($value);
      $maxCount = $count;
    } elseif ($count == $maxCount) {
      $modes[] = $value;