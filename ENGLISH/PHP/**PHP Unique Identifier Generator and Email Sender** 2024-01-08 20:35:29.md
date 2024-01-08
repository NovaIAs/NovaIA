```php
<?php
// Define a function to generate a random string of a given length
function generateRandomString($length) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $charactersLength = strlen($characters);
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, $charactersLength - 1)];
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

// Get the current timestamp
$timestamp = time();

// Generate a random string of 10 characters
$randomString = generateRandomString(10);

// Create a unique identifier using the timestamp and random string
$uniqueIdentifier = $timestamp . '-' . $randomString;

// Send an email to the specified recipient with the unique identifier
sendEmail('recipient@example.com', 'Unique Identifier', 'Your unique identifier is: ' . $uniqueIdentifier);

// Display the unique identifier on the web page
echo 'Your unique identifier is: ' . $uniqueIdentifier;
?>
```

This code performs the following tasks:

1. It defines a function called `generateRandomString()` that generates a random string of a specified length. The function uses the `rand()` function to generate a random index within the range of the characters string, and then it appends the character at that index to the `$randomString` variable. This process is repeated until the desired length of the random string is reached.

2. It defines a function called `sendEmail()` that sends an email to a specified recipient with a specified subject and message. The function uses the `mail()` function to send the email.

3. It gets the current timestamp using the `time()` function and stores it in the `$timestamp` variable.

4. It generates a random string of 10 characters using the `generateRandomString()` function and stores it in the `$randomString` variable.

5. It creates a unique identifier by concatenating the `$timestamp` and `$randomString` variables with a hyphen in between.

6. It sends an email to the specified recipient with the unique identifier using the `sendEmail()` function. The email subject is "Unique Identifier" and the email message contains the unique identifier.

7. It displays the unique identifier on the web page using the `echo` statement.

This code demonstrates the use of functions, string concatenation, the `time()` function, and the `mail()` function to generate a unique identifier, send an email, and display the unique identifier on the web page.