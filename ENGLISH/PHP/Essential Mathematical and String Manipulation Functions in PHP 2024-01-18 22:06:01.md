```php
<?php
// Define a function to calculate the factorial of a number
function factorial($n) {
    // Base case: factorial of 0 is 1
    if ($n == 0) {
        return 1;
    }
    // Recursive case: calculate the factorial of n-1 and multiply it by n
    else {
        return $n * factorial($n-1);
    }
}

// Define a function to check if a number is prime
function is_prime($n) {
    // Base case: 1 is not prime
    if ($n == 1) {
        return false;
    }
    // Loop through all numbers from 2 to the square root of n
    for ($i = 2; $i <= sqrt($n); $i++) {
        // If n is divisible by any number from 2 to its square root, it is not prime
        if ($n % $i == 0) {
            return false;
        }
    }
    // If n is divisible by no number from 2 to its square root, it is prime
    return true;
}

// Define a function to calculate the Fibonacci sequence up to a given number
function fibonacci($n) {
    // Base cases: the first two numbers in the Fibonacci sequence are 0 and 1
    if ($n == 0) {
        return 0;
    }
    if ($n == 1) {
        return 1;
    }
    // Recursive case: calculate the nth Fibonacci number by adding the (n-1)th and (n-2)th Fibonacci numbers
    else {
        return fibonacci($n-1) + fibonacci($n-2);
    }
}

// Define a function to reverse a string
function reverse_string($str) {
    // Base case: an empty string is its own reverse
    if ($str == "") {
        return "";
    }
    // Recursive case: reverse the substring from the second character to the end, and append the first character to the end of the reversed substring
    else {
        return reverse_string(substr($str, 1)) . $str[0];
    }
}

// Define a function to check if a string is a palindrome
function is_palindrome($str) {
    // Remove all non-alphanumeric characters from the string and convert it to lowercase
    $str = preg_replace('/[^a-zA-Z0-9]/', '', strtolower($str));
    // Check if the string is the same forwards and backwards
    return $str == reverse_string($str);
}

// Define a function to find the longest common substring of two strings
function longest_common_substring($str1, $str2) {
    // Find the length of the two strings
    $len1 = strlen($str1);
    $len2 = strlen($str2);

    // Create a matrix to store the lengths of the longest common substrings of the two strings
    $lcs = array();
    for ($i = 0; $i <= $len1; $i++) {
        $lcs[$i] = array();
        for ($j = 0; $j <= $len2; $j++) {
            $lcs[$i][$j] = 0;
        }
    }

    // Fill the matrix with the lengths of the longest common substrings of the two strings
    for ($i = 1; $i <= $len1; $i++) {
        for ($j = 1; $j <= $len2; $j++) {
            if ($str1[$i-1] == $str2[$j-1]) {
                $lcs[$i][$j] = $lcs[$i-1][$j-1] + 1;
            }
            else {
                $lcs[$i][$j] = 0;
            }
        }
    }

    // Find the maximum value in the matrix
    $max_lcs = 0;
    $max_i = 0;
    $max_j = 0;
    for ($i = 1; $i <= $len1; $i++) {
        for ($j = 1; $j <= $len2; $j++) {
            if ($lcs[$i][$j] > $max_lcs) {
                $max_lcs = $lcs[$i][$j];
                $max_i = $i;
                $max_j = $j;
            }
        }
    }

    // Extract the longest common substring from the two strings
    $lcs_str = "";
