```php
// Define a function to generate a random string of a given length
function generateRandomString($length) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, strlen($characters) - 1)];
    }
    return $randomString;
}

// Define a function to send an email with a given subject, body, and recipient
function sendEmail($subject, $body, $recipient) {
    // Set the email headers
    $headers = 'From: noreply@example.com' . "\r\n" .
        'Reply-To: noreply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();

    // Send the email
    mail($recipient, $subject, $body, $headers);
}

// Define a function to log a message to a file
function logMessage($message) {
    // Open the log file
    $logFile = fopen('log.txt', 'a');

    // Write the message to the log file
    fwrite($logFile, $message . "\n");

    // Close the log file
    fclose($logFile);
}

// Generate a random string of length 10
$randomString = generateRandomString(10);

// Send an email with the subject "Random String", the body "Your random string is: $randomString", and the recipient "john.doe@example.com"
sendEmail("Random String", "Your random string is: $randomString", "john.doe@example.com");

// Log a message to the log file
logMessage("Generated random string: $randomString");
```

This code defines three functions: `generateRandomString`, `sendEmail`, and `logMessage`. The `generateRandomString` function generates a random string of a given length. The `sendEmail` function sends an email with a given subject, body, and recipient. The `logMessage` function logs a message to a file.

The code then generates a random string of length 10, sends an email with the subject "Random String", the body "Your random string is: $randomString", and the recipient "john.doe@example.com", and logs a message to the log file.