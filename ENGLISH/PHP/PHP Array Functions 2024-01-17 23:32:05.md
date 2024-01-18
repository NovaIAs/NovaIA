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

// Define the function to calculate the sum of an array
function array_sum($array) {
    $sum = 0;
    foreach ($array as $value) {
        $sum += $value;
    }
    return $sum;
}

// Define the function to calculate the average of an array
function array_average($array) {
    $sum = array_sum($array);
    $count = count($array);
    return $sum / $count;
}

// Define the function to find the maximum value in an array
function array_max($array) {
    $max = $array[0];
    foreach ($array as $value) {
        if ($value > $max) {
            $max = $value;
        }
    }
    return $max;
}

// Define the function to find the minimum value in an array
function array_min($array) {
    $min = $array[0];
    foreach ($array as $value) {
        if ($value < $min) {
            $min = $value;
        }
    }
    return $min;
}

// Define the function to search for a value in an array
function array_search($value, $array) {
    foreach ($array as $key => $array_value) {
        if ($array_value == $value) {
            return $key;
        }
    }
    return -1;
}

// Define the function to sort an array in ascending order
function array_sort($array) {
    sort($array);
    return $array;
}

// Define the function to sort an array in descending order
function array_sort_desc($array) {
    rsort($array);
    return $array;
}

// Define the function to shuffle an array
function array_shuffle($array) {
    shuffle($array);
    return $array;
}

// Define the function to reverse an array
function array_reverse($array) {
    $reversed_array = array();
    for ($i = count($array) - 1; $i >= 0; $i--) {
        $reversed_array[] = $array[$i];
    }
    return $reversed_array;
}

// Define the function to merge two arrays
function array_merge($array1, $array2) {
    $merged_array = array_merge($array1, $array2);
    return $merged_array;
}

// Define the function to intersect two arrays
function array_intersect($array1, $array2) {
    $intersected_array = array_intersect($array1, $array2);
    return $intersected_array;
}

// Define the function to get the unique values in an array
function array_unique($array) {
    $unique_array = array_unique($array);
    return $unique_array;
}

// Define the function to count the number of occurrences of a value in an array
function array_count_values($array) {
    $counted_array = array_count_values($array);
    return $counted_array;
}

// Define the function to create a new array by applying a callback function to each element of an array
function array_map($callback, $array) {
    $mapped_array = array_map($callback, $array);
    return $mapped_array;
}

// Define the function to filter an array using a callback function
function array_filter($callback, $array) {
    $filtered_array = array_filter($array, $callback);
    return $filtered_array;
}

// Define the function to reduce an array to a single value using a callback function
function array_reduce($callback, $array, $initial_value = null) {
    $reduced_value = array_reduce($array, $callback, $initial_value);
    return $reduced_value;
}

// Define the function to check if an array is associative
function is_associative_array($array) {
    return array_keys($array) !== range(0, count($array) - 1);
}

// Define the function to convert an associative array to a list of key-value pairs
function array_to_list($array) {
    $list = array();
    foreach ($array as $key => $value) {
        $list[] = array($key, $value);
    }
    return $list;
}

// Define the function to convert a list of key-value pairs to an associative array
function list_to_array($list) {
    $array = array();
    foreach ($list as $pair) {
        $array[$pair[0]] = $pair[1];