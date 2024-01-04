```php
<?php

// Define the function to calculate the factorial of a number
function factorial($n) {
    if ($n === 0) {
        return 1;
    } else {
        return $n * factorial($n - 1);
    }
}

// Define the function to calculate the Fibonacci sequence
function fibonacci($n) {
    if ($n <= 1) {
        return $n;
    } else {
        return fibonacci($n - 1) + fibonacci($n - 2);
    }
}

// Define the function to check if a number is prime
function isPrime($n) {
    if ($n <= 1) {
        return false;
    } else if ($n <= 3) {
        return true;
    } else if ($n % 2 === 0 || $n % 3 === 0) {
        return false;
    } else {
        for ($i = 5; $i * $i <= $n; $i += 6) {
            if ($n % $i === 0 || $n % ($i + 2) === 0) {
                return false;
            }
        }
        return true;
    }
}

// Define the function to calculate the greatest common divisor of two numbers
function gcd($a, $b) {
    if ($b === 0) {
        return $a;
    } else {
        return gcd($b, $a % $b);
    }
}

// Define the function to calculate the least common multiple of two numbers
function lcm($a, $b) {
    return ($a * $b) / gcd($a, $b);
}

// Define the function to calculate the sum of digits of a number
function sumOfDigits($n) {
    if ($n === 0) {
        return 0;
    } else {
        return $n % 10 + sumOfDigits($n / 10);
    }
}

// Define the function to calculate the reverse of a number
function reverseNumber($n) {
    if ($n === 0) {
        return 0;
    } else {
        return ($n % 10) * pow(10, floor(log10($n))) + reverseNumber($n / 10);
    }
}

// Define the function to check if a number is a palindrome
function isPalindrome($n) {
    return $n === reverseNumber($n);
}

// Define the function to check if a number is an Armstrong number
function isArmstrongNumber($n) {
    $digits = strlen($n);
    $sum = 0;
    for ($i = 0; $i < $digits; $i++) {
        $sum += pow(substr($n, $i, 1), $digits);
    }
    return $sum === $n;
}

// Define the function to check if a number is a perfect number
function isPerfectNumber($n) {
    $sum = 0;
    for ($i = 1; $i < $n; $i++) {
        if ($n % $i === 0) {
            $sum += $i;
        }
    }
    return $sum === $n;
}

// Define the function to check if a number is a deficient number
function isDeficientNumber($n) {
    $sum = 0;
    for ($i = 1; $i < $n; $i++) {
        if ($n % $i === 0) {
            $sum += $i;
        }
    }
    return $sum < $n;
}

// Define the function to check if a number is an abundant number
function isAbundantNumber($n) {
    $sum = 0;
    for ($i = 1; $i < $n; $i++) {
        if ($n % $i === 0) {
            $sum += $i;
        }
    }
    return $sum > $n;
}

// Define the function to check if a number is a Harshad number
function isHarshadNumber($n) {
    $sum = 0;
    $num = $n;
    while ($num > 0) {
        $sum += $num % 10;
        $num = floor($num / 10);
    }
    return $n % $sum === 0;
}

// Define the function to check if a number is a Niven number
function isNivenNumber($n) {
    $sum = 0;
    $num = $n;
    while ($num > 0) {
        $sum += $num % 10;
        $num = floor($num / 10);
    }
    return $n % $sum === 0;
}

// Define the function to check if a number is a Kaprekar number
function isKaprekarNumber($n) {
    $square = $n * $n;
    $str_square = strval($square);
    $length = strlen($str_square);
    $left = substr($str_square, 0, $length / 2);
    $right = substr($str_square, $length / 2);
    return $left + $right === $n;
}

```

This code contains a collection of mathematical functions that can be used to perform various calculations on numbers. The functions include:

* Factorial: Calculates the factorial of a number.
* Fibonacci: Calculates the nth Fibonacci number.
* Is Prime: Checks if a number is prime.
* Greatest Common Divisor: Calculates the greatest common divisor of two numbers.
* Least Common Multiple: Calculates the least common multiple of two numbers.
* Sum of Digits: Calculates the sum of digits of a number.
* Reverse Number: Calculates the reverse of a number.
* Is Palindrome: Checks if a number is a palindrome.
* Is Armstrong Number: Checks if a number is an Armstrong number.
* Is Perfect Number: Checks if a number is a perfect number.
* Is Deficient Number: Checks if a number is a deficient number.
* Is Abundant Number: Checks if a number is an abundant number.
* Is Harshad Number: Checks if a number is a Harshad number.
* Is Niven Number: Checks if a number is a Niven number.
* Is Kaprekar Number: Checks if a number is a Kaprekar number.