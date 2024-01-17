```php
<?php
// Define a function that takes an array of integers as input and returns the sum of the array
function sum_array($array) {
  // Initialize the sum variable to zero
  $sum = 0;

  // Iterate over the array and add each element to the sum variable
  foreach ($array as $element) {
    $sum += $element;
  }

  // Return the sum of the array
  return $sum;
}

// Define a function that takes a string as input and returns the number of words in the string
function count_words($string) {
  // Split the string into an array of words
  $words = explode(" ", $string);

  // Return the number of elements in the array (which is the number of words in the string)
  return count($words);
}

// Define a function that takes an array of strings as input and returns the longest string in the array
function longest_string($array) {
  // Initialize the longest string variable to the first element of the array
  $longest_string = $array[0];

  // Iterate over the array and check if each element is longer than the longest string
  foreach ($array as $element) {
    if (strlen($element) > strlen($longest_string)) {
      // If the element is longer than the longest string, update the longest string variable
      $longest_string = $element;
    }
  }

  // Return the longest string in the array
  return $longest_string;
}

// Define a function that takes a string as input and returns the number of vowels in the string
function count_vowels($string) {
  // Initialize the number of vowels variable to zero
  $num_vowels = 0;

  // Iterate over the string and check if each character is a vowel
  for ($i = 0; $i < strlen($string); $i++) {
    $char = $string[$i];
    if ($char == 'a' || $char == 'e' || $char == 'i' || $char == 'o' || $char == 'u') {
      // If the character is a vowel, increment the number of vowels variable
      $num_vowels++;
    }
  }

  // Return the number of vowels in the string
  return $num_vowels;
}

// Define a function that takes an array of integers as input and returns the average of the array
function average($array) {
  // Get the sum of the array
  $sum = sum_array($array);

  // Get the number of elements in the array
  $count = count($array);

  // Calculate the average by dividing the sum by the number of elements
  $average = $sum / $count;

  // Return the average of the array
  return $average;
}

// Define a function that takes an array of strings as input and returns a new array containing the unique strings from the input array
function unique_array($array) {
  // Create a new array to store the unique strings
  $unique_array = array();

  // Iterate over the input array
  foreach ($array as $element) {
    // If the element is not already in the unique array, add it to the array
    if (!in_array($element, $unique_array)) {
      $unique_array[] = $element;
    }
  }

  // Return the unique array
  return $unique_array;
}

// Define a function that takes a string as input and returns a new string with the first character of each word capitalized
function capitalize_words($string) {
  // Split the string into an array of words
  $words = explode(" ", $string);

  // Iterate over the array of words and capitalize the first character of each word
  for ($i = 0; $i < count($words); $i++) {
    $words[$i] = ucfirst($words[$i]);
  }

  // Join the array of words back into a string
  $new_string = implode(" ", $words);

  // Return the new string
  return $new_string;
}

// Define a function that takes a string as input and returns a new string with all the punctuation removed
function remove_punctuation($string) {
  // Create a regular expression to match punctuation characters
  $punctuation = "[[:punct:]]";

  // Replace all the punctuation characters in the string with an empty string
  $new_string = preg_replace($punctuation, "", $string);

  // Return the new string
  return $new_string;
}

// Define a function that takes a string as input and returns a new string with all the numbers removed
function remove_numbers($string) {
  // Create a regular expression to match numbers
  $numbers = "[0-9]";

  // Replace all the numbers in the string with an empty string
  $new_string = preg_replace($numbers, "", $string);

  // Return the new string
  return $new_string;
}

// Define a function that takes a string as input and returns a