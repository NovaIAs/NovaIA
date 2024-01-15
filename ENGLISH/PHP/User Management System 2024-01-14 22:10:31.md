```php
<?php

// Define a function to generate a random string of a given length
function generateRandomString($length = 10) {
    // Create a string of all possible characters
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    // Initialize the random string
    $randomString = '';
    // Loop through the desired length of the string
    for ($i = 0; $i < $length; $i++) {
        // Get a random index from the characters string
        $randomIndex = rand(0, strlen($characters) - 1);
        // Add the character at the random index to the random string
        $randomString .= $characters[$randomIndex];
    }
    // Return the random string
    return $randomString;
}

// Define a function to send an email
function sendEmail($to, $subject, $message) {
    // Set the email headers
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();
    // Send the email
    mail($to, $subject, $message, $headers);
}

// Define a function to create a new user account
function createUserAccount($username, $password, $email) {
    // Connect to the database
    $db = new PDO('mysql:host=localhost;dbname=users', 'root', '');
    // Prepare the SQL statement to insert the new user into the database
    $stmt = $db->prepare('INSERT INTO users (username, password, email) VALUES (?, ?, ?)');
    // Bind the parameters to the SQL statement
    $stmt->bindParam(1, $username);
    $stmt->bindParam(2, $password);
    $stmt->bindParam(3, $email);
    // Execute the SQL statement
    $stmt->execute();
    // Close the database connection
    $db = null;
}

// Define a function to log a user in
function loginUser($username, $password) {
    // Connect to the database
    $db = new PDO('mysql:host=localhost;dbname=users', 'root', '');
    // Prepare the SQL statement to select the user from the database
    $stmt = $db->prepare('SELECT * FROM users WHERE username = ? AND password = ?');
    // Bind the parameters to the SQL statement
    $stmt->bindParam(1, $username);
    $stmt->bindParam(2, $password);
    // Execute the SQL statement
    $stmt->execute();
    // Fetch the user data from the database
    $user = $stmt->fetch();
    // Close the database connection
    $db = null;
    // Return the user data
    return $user;
}

// Define a function to get the current user's data
function getCurrentUser() {
    // Get the current user's username from the session
    $username = $_SESSION['username'];
    // Connect to the database
    $db = new PDO('mysql:host=localhost;dbname=users', 'root', '');
    // Prepare the SQL statement to select the user from the database
    $stmt = $db->prepare('SELECT * FROM users WHERE username = ?');
    // Bind the parameters to the SQL statement
    $stmt->bindParam(1, $username);
    // Execute the SQL statement
    $stmt->execute();
    // Fetch the user data from the database
    $user = $stmt->fetch();
    // Close the database connection
    $db = null;
    // Return the user data
    return $user;
}

// Define a function to update the current user's data
function updateCurrentUser($username, $password, $email) {
    // Connect to the database
    $db = new PDO('mysql:host=localhost;dbname=users', 'root', '');
    // Prepare the SQL statement to update the user in the database
    $stmt = $db->prepare('UPDATE users SET username = ?, password = ?, email = ? WHERE username = ?');
    // Bind the parameters to the SQL statement
    $stmt->bindParam(1, $username);
    $stmt->bindParam(2, $password);
    $stmt->bindParam(3, $email);
    $stmt->bindParam(4, $_SESSION['username']);
    // Execute the SQL statement
    $stmt->execute();
    // Close the database connection
    $db = null;
}

// Define a function to delete the current user's account
function deleteCurrentUser() {
    // Connect to the database
    $db = new PDO('mysql:host=localhost;dbname=users', 'root', '');
    // Prepare the SQL statement to delete the user from the database
    $stmt = $db->prepare('DELETE FROM users WHERE username = ?');
    // Bind the parameters to the SQL statement
    $stmt->bindParam(1, $_SESSION['username']);
    // Execute the SQL statement
    $stmt->execute();
    // Close the database connection
    $db = null;
    // Destroy the session
    session_destroy();
}

// Define a function to check if a user is logged in
function isLoggedIn() {
    // Check if the user is logged in
    return isset($_SESSION['username']);
}

// Define a function to redirect the user to a given page
function redirect($page) {
    // Redirect the user to the given page
    header('Location: ' . $page);
    exit;
}

// Start the session
session_start();

// Check if the user is logged in
if (isLoggedIn()) {
    // Get the current user's data
    $user = getCurrentUser();
}

// Handle the form submission
if (isset($_POST['submit'])) {
    // Get the form data
    $username = $_POST['username'];
    $password = $_POST['password'];
    $email = $_POST['email'];

    // Check if the user is creating a new account