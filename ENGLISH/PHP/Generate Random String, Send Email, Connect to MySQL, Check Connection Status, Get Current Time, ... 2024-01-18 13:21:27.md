```php
<?php
// Define the function to generate a random string
function generateRandomString($length = 10) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $randomString;
}

// Define the function to send an email
function sendEmail($to, $subject, $message) {
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();
    mail($to, $subject, $message, $headers);
}

// Create a database connection
$mysqli = new mysqli("localhost", "username", "password", "database");

// Check if the connection was successful
if ($mysqli->connect_errno) {
    echo "Failed to connect to MySQL: (" . $mysqli->connect_errno . ") " . $mysqli->connect_error;
}

// Get the current time
$currentTime = date("Y-m-d H:i:s");

// Insert a new record into the database
$stmt = $mysqli->prepare("INSERT INTO table_name (column1, column2, column3) VALUES (?, ?, ?)");
$stmt->bind_param("sss", $randomString, $currentTime, $message);
$stmt->execute();

// Close the database connection
$mysqli->close();

// Send an email to the user
sendEmail($to, $subject, $message);

// Display a success message
echo "Your message has been sent successfully.";
?>
```

This code performs the following tasks:

1. Defines a function called `generateRandomString()` that generates a random string of a specified length. It uses a loop to generate the string by randomly selecting characters from a predefined set of characters.


2. Defines a function called `sendEmail()` that sends an email to the specified recipient with the given subject and message. It uses the `mail()` function to send the email.


3. Establishes a connection to a MySQL database using the `mysqli` class. It provides the hostname, username, password, and database name as arguments to the constructor.


4. Checks if the connection to the database was successful. If there was an error connecting to the database, it displays an error message.


5. Gets the current time using the `date()` function and formats it in the "Y-m-d H:i:s" format.


6. Inserts a new record into the specified table in the database using a prepared statement. The prepared statement protects against SQL injection attacks and ensures data integrity.


7. Closes the database connection using the `close()` method of the `mysqli` object.


8. Calls the `sendEmail()` function to send an email to the user with the message that was inserted into the database.


9. Displays a success message to the user indicating that their message was sent successfully.