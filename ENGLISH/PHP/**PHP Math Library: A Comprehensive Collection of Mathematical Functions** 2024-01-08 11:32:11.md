```php
<?php

// Define the function to calculate the factorial of a number
function factorial($n) {
  if ($n == 0) {
    return 1;
  } else {
    return $n * factorial($n - 1);
  }
}

// Define the function to calculate the Fibonacci sequence
function fibonacci($n) {
  if ($n == 0) {
    return 0;
  } else if ($n == 1) {
    return 1;
  } else {
    return fibonacci($n - 1) + fibonacci($n - 2);
  }
}

// Define the function to calculate the greatest common divisor of two numbers
function gcd($a, $b) {
  if ($b == 0) {
    return $a;
  } else {
    return gcd($b, $a % $b);
  }
}

// Define the function to calculate the least common multiple of two numbers
function lcm($a, $b) {
  return ($a * $b) / gcd($a, $b);
}

// Define the function to check if a number is prime
function is_prime($n) {
  if ($n <= 1) {
    return false;
  } else if ($n <= 3) {
    return true;
  } else if ($n % 2 == 0 || $n % 3 == 0) {
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

// Define the function to find the prime factors of a number
function prime_factors($n) {
  $factors = array();
  $divisor = 2;
  while ($n > 1) {
    if ($n % $divisor == 0) {
      $factors[] = $divisor;
      $n /= $divisor;
    } else {
      $divisor++;
    }
  }
  return $factors;
}

// Define the function to calculate the sum of the digits of a number
function sum_of_digits($n) {
  $sum = 0;
  while ($n > 0) {
    $sum += $n % 10;
    $n /= 10;
  }
  return $sum;
}

// Define the function to check if a number is a palindrome
function is_palindrome($n) {
  $str = (string) $n;
  return $str == strrev($str);
}

// Define the function to find the reverse of a number
function reverse($n) {
  $reversed = 0;
  while ($n > 0) {
    $reversed = $reversed * 10 + $n % 10;
    $n /= 10;
  }
  return $reversed;
}

// Define the function to find the square root of a number
function square_root($n) {
  if ($n < 0) {
    return "Cannot find the square root of a negative number.";
  } else if ($n == 0) {
    return 0;
  } else {
    $x = $n / 2;
    while (abs($x - $n / $x) > 0.001) {
      $x = ($x + $n / $x) / 2;
    }
    return $x;
  }
}

// Define the function to find the cube root of a number
function cube_root($n) {
  if ($n < 0) {
    return "Cannot find the cube root of a negative number.";
  } else if ($n == 0) {
    return 0;
  } else {
    $x = $n / 3;
    while (abs($x - $n / $x**2) > 0.001) {
      $x = ($x + $n / $x**2) / 2;
    }
    return $x;
  }
}

// Define the function to find the n-th root of a number
function nth_root($n, $x) {
  if ($n < 0) {
    return "Cannot find the n-th root of a negative number.";
  } else if ($n == 0) {
    return "Cannot find the 0-th root of a number.";
  } else if ($x < 0) {
    return "Cannot find the n-th root of a negative number.";
  } else if ($x == 0) {
    return 0;
  } else {
    $y = $x / $n;
    while (abs($y - $x / $y**(