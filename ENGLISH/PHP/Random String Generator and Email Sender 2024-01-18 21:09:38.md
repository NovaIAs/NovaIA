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
function sendEmail($to, $subject, $body) {
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();

    mail($to, $subject, $body, $headers);
}

// Generate a random string of length 10
$randomString = generateRandomString(10);

// Send an email to the user with the random string as the subject and body
sendEmail('user@example.com', 'Your Random String', $randomString);

// Store the random string in a database
$conn = new mysqli('localhost', 'username', 'password', 'database');
$stmt = $conn->prepare('INSERT INTO random_strings (random_string) VALUES (?)');
$stmt->bind_param('s', $randomString);
$stmt->execute();
$stmt->close();
$conn->close();

// Display the random string to the user
echo 'Your random string is: ' . $randomString;

?>
```

**Explanation:**

This code generates a random string of length 10, sends it to the user via email, stores it in a database, and then displays it to the user.

1. The `generateRandomString()` function generates a random string of a given length. It does this by selecting characters randomly from the string `'0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ'` and concatenating them together.

2. The `sendEmail()` function sends an email to a given recipient with a given subject and body. It uses the PHP `mail()` function to send the email.

3. The `generateRandomString()` function is called to generate a random string of length 10.

4. The `sendEmail()` function is called to send the random string to the user via email.

5. A connection to the database is established using the `mysqli` class.

6. A prepared statement is created to insert the random string into the `random_strings` table in the database.

7. The prepared statement is executed to insert the random string into the database.

8. The prepared statement and the database connection are closed.

9. The random string is displayed to the user using `echo`.