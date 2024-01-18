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

// Define a function to calculate the nth Fibonacci number
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

// Define a function to convert a number from one base to another
function base_convert($number, $from_base, $to_base) {
    $result = "";
    while ($number > 0) {
        $remainder = $number % $to_base;
        $result = base_convert($remainder, 10, $to_base) . $result;
        $number /= $to_base;
    }
    return $result;
}

// Define a function to check if a string is a palindrome
function is_palindrome($string) {
    $string = strtolower($string);
    $string = preg_replace('/[^a-z0-9]/', '', $string);
    return $string == strrev($string);
}

// Define a function to generate a random password
function generate_password($length = 8) {
    $characters = 'abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()';
    $password = '';
    for ($i = 0; $i < $length; $i++) {
        $password .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $password;
}

// Define a function to send an email
function send_email($to, $subject, $message) {
    $headers = 'From: webmaster@example.com' . "\r\n" .
        'Reply-To: webmaster@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();
    mail($to, $subject, $message, $headers);
}

// Define a function to upload a file
function upload_file($file) {
    if (isset($file['name']) && !empty($file['name'])) {
        $allowed_extensions = array('jpg', 'jpeg', 'png', 'gif');
        $file_extension = pathinfo($file['name'], PATHINFO_EXTENSION);
        if (in_array($file_extension, $allowed_extensions)) {
            if ($file['size'] <= 1000000) {
                $target_directory = 'uploads/';
                $target_file = $target_directory . basename($file['name']);
                if (move_uploaded_file($file['tmp_name'], $target_file)) {
                    return $target_file;
                } else {
                    return false;
                }
            } else {
                return 'File size is too large.';
            }
        } else {
            return 'Invalid file type.';
        }
    } else {
        return 'No file selected.';
    }
}

// Define a function to create a database connection
function connect_to_database() {
    $servername = "localhost";
    $username = "root";
    $password = "";
    $database = "mydb";
    $conn = new mysqli($servername, $username, $password, $database);
    if ($conn->connect_error) {
        die("Connection failed: " . $conn->connect_error);
    }
    return $conn;
}

// Define a function to execute a SQL query
function execute_query($conn, $query) {
    $result = $conn->query($query);
    if (!$result) {
        die("Error executing query: " . $conn->error);
    }
    return $result;
}

// Define a function to close a database connection
function close_database_connection($conn) {
    $conn->close();
}

// Usage examples
$factorial_result = factorial(5); // Output: 120
$fibonacci_result = fibonacci(8); // Output: 21
$is_prime_result = is_prime(17); // Output: true
$gcd_result = gcd(36, 24); // Output: 12
$lcm_result = lcm(36, 24); // Output: 72
$base_convert_result = base_convert(100, 10, 2); // Output: 1100100
$is_palindrome_result = is_palindrome("Racecar"); // Output: true
$password_result = generate_password(); // Output: a random password

// Send an email
send_email('