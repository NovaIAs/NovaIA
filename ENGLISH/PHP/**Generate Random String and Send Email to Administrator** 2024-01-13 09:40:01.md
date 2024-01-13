```php
<?php
// Define a function to generate a random string of a given length
function generateRandomString($length) {
  $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
  $randomString = '';
  for ($i = 0; $i < $length; $i++) {
    $randomString .= $characters[rand(0, strlen($characters) - 1)];
  }
  return $randomString;
}

// Define a function to send an email
function sendEmail($to, $subject, $message) {
  $headers = 'From: noreply@example.com' . "\r\n" .
             'Reply-To: noreply@example.com' . "\r\n" .
             'X-Mailer: PHP/' . phpversion();

  mail($to, $subject, $message, $headers);
}

// Define a function to connect to a MySQL database
function connectToDatabase() {
  $servername = "localhost";
  $username = "username";
  $password = "password";
  $dbname = "database";

  // Create connection
  $conn = new mysqli($servername, $username, $password, $dbname);

  // Check connection
  if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
  }

  return $conn;
}

// Define a function to execute a SQL query
function executeQuery($conn, $sql) {
  $result = $conn->query($sql);

  if ($result->num_rows > 0) {
    // Output data of each row
    while($row = $result->fetch_assoc()) {
      echo "id: " . $row["id"] . " - Name: " . $row["name"] . " - Email: " . $row["email"] . "<br>";
    }
  } else {
    echo "0 results";
  }

  $conn->close();
}

// Generate a random string of length 10
$randomString = generateRandomString(10);

// Send an email to the administrator with the random string as the subject
sendEmail("admin@example.com", "New Random String", $randomString);

// Connect to the MySQL database
$conn = connectToDatabase();

// Execute a SQL query to insert the random string into the database
$sql = "INSERT INTO random_strings (random_string) VALUES ('$randomString')";
executeQuery($conn, $sql);
?>
```

This code performs the following tasks:

1. It defines a function called `generateRandomString` that generates a random string of a given length.
2. It defines a function called `sendEmail` that sends an email to a given recipient with a given subject and message.
3. It defines a function called `connectToDatabase` that connects to a MySQL database with a given hostname, username, password, and database name.
4. It defines a function called `executeQuery` that executes a SQL query on a given MySQL connection and prints the results.
5. It generates a random string of length 10 using the `generateRandomString` function.
6. It sends an email to the administrator with the random string as the subject using the `sendEmail` function.
7. It connects to the MySQL database using the `connectToDatabase` function.
8. It executes a SQL query to insert the random string into the database using the `executeQuery` function.