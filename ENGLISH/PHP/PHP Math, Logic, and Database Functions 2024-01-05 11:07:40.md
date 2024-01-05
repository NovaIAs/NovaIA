<?php
// Define the function to calculate the factorial of a number
function factorial($number) {
  if ($number < 0) {
    throw new Exception("Factorial is not defined for negative numbers");
  }
  if ($number == 0) {
    return 1;
  } else {
    return $number * factorial($number - 1);
  }
}

// Define the function to calculate the Fibonacci sequence
function fibonacci($number) {
  if ($number < 0) {
    throw new Exception("Fibonacci sequence is not defined for negative numbers");
  }
  if ($number == 0) {
    return 0;
  } elseif ($number == 1) {
    return 1;
  } else {
    return fibonacci($number - 1) + fibonacci($number - 2);
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
function is_prime($number) {
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

// Define the function to generate a random string
function generate_random_string($length) {
  $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $random_string = '';
  for ($i = 0; $i < $length; $i++) {
    $random_string .= $characters[rand(0, strlen($characters) - 1)];
  }
  return $random_string;
}

// Define the function to send an email
function send_email($to, $subject, $message) {
  $headers = 'From: no-reply@example.com' . "\r\n" .
    'Reply-To: no-reply@example.com' . "\r\n" .
    'X-Mailer: PHP/' . phpversion();

  return mail($to, $subject, $message, $headers);
}

// Define the function to connect to a database
function connect_to_database($hostname, $username, $password, $database_name) {
  $connection = new mysqli($hostname, $username, $password, $database_name);
  if ($connection->connect_error) {
    throw new Exception("Failed to connect to database: " . $connection->connect_error);
  }
  return $connection;
}

// Define the function to execute a query on a database
function execute_query($connection, $query) {
  $result = $connection->query($query);
  if ($result === false) {
    throw new Exception("Failed to execute query: " . $connection->error);
  }
  return $result;
}

// Define the function to fetch the results of a query
function fetch_results($result) {
  $rows = array();
  while ($row = $result->fetch_assoc()) {
    $rows[] = $row;
  }
  return $rows;
}

// Define the function to close a database connection
function close_database_connection($connection) {
  $connection->close();
}

// Usage example

// Calculate the factorial of a number
$number = 5;
$factorial = factorial($number);
echo "The factorial of $number is $factorial" . PHP_EOL;

// Calculate the Fibonacci sequence
$number = 10;
$fibonacci = fibonacci($number);
echo "The $numberth number in the Fibonacci sequence is $fibonacci" . PHP_EOL;

// Calculate the greatest common divisor of two numbers
$a = 12;
$b = 18;
$gcd = gcd($a, $b);
echo "The greatest common divisor of $a and $b is $gcd" . PHP_EOL;

// Calculate the least common multiple of two numbers
$a = 12;
$b = 18;
$lcm = lcm($a, $b);
echo "The least common multiple of $a and $b is $lcm" . PHP_EOL;

// Check if a number is prime
$number = 17;
$is_prime = is_prime($number);
if ($is_prime) {
  echo "$number is a prime number" . PHP_EOL;
} else {
  echo "$number is not a prime number" . PHP_EOL;
}

// Generate a random string
$length = 10;
$random_string = generate_random_string($length);
echo "Generated random string: $random_string" . PHP_EOL;

// Send an email
$to = 'example@example.com';
$subject = 'Hello from PHP';
$message = 'This is an email sent from PHP';
$sent = send_email($to, $subject, $message);
if ($sent) {
  echo "Email sent successfully" . PHP_EOL;
} else {
  echo "Failed to send email" . PHP_EOL;
}

// Connect to a database
$hostname = 'localhost';
$username = 'root';
$password = 'password';
$database_name = 'example_database';
$connection = connect_to_database($hostname, $username, $password, $database_name);

// Execute a query on the database
$query = 'SELECT * FROM users';
$result = execute_query($connection, $query);

// Fetch the results of the query
$users = fetch_results($result);

// Close the database connection
close_database_connection($connection);

// Display the results of the query
foreach ($users as $user) {
  echo "User: $user[name] ($user[email])" . PHP_EOL;
}
?>