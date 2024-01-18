```php
<?php

// Define the function to generate a random string
function generateRandomString($length = 10) {
    $characters = '0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ';
    $charactersLength = strlen($characters);
    $randomString = '';
    for ($i = 0; $i < $length; $i++) {
        $randomString .= $characters[rand(0, $charactersLength - 1)];
    }
    return $randomString;
}

// Define the function to send an email
function sendEmail($to, $subject, $message) {
    $headers = 'From: no-reply@example.com' . "\r\n" .
        'Reply-To: no-reply@example.com' . "\r\n" .
        'X-Mailer: PHP/' . phpversion();

    mail($to, $subject, $message, $headers);
}

// Define the function to process the form
function processForm() {
    // Get the form data
    $name = $_POST['name'];
    $email = $_POST['email'];
    $message = $_POST['message'];

    // Validate the form data
    if (empty($name) || empty($email) || empty($message)) {
        // Display an error message
        echo 'Please fill in all the fields.';
        return;
    }

    // Generate a random string for the token
    $token = generateRandomString();

    // Save the token and the user's email address in the database
    $query = "INSERT INTO tokens (token, email) VALUES ('$token', '$email')";
    $result = mysqli_query($connection, $query);

    if (!$result) {
        // Display an error message
        echo 'There was a problem saving your information. Please try again.';
        return;
    }

    // Send the email with the link to the token
    $subject = 'Verify your email address';
    $message = 'Please click on the following link to verify your email address:' . "\r\n" .
        'http://example.com/verify.php?token=' . $token;
    sendEmail($email, $subject, $message);

    // Display a success message
    echo 'Thank you for registering. Please check your email for a verification link.';
}

// Process the form if the submit button was clicked
if (isset($_POST['submit'])) {
    processForm();
}

?>

<!DOCTYPE html>
<html>
<head>
    <title>Contact Form</title>
</head>
<body>
    <h1>Contact Form</h1>
    <form method="post">
        <label for="name">Name:</label>
        <input type="text" name="name" id="name">

        <label for="email">Email:</label>
        <input type="email" name="email" id="email">

        <label for="message">Message:</label>
        <textarea name="message" id="message"></textarea>

        <input type="submit" name="submit" value="Submit">
    </form>
</body>
</html>
```

This code is a PHP script that implements a registration form with email verification. It defines a function to generate a random string, a function to send an email, and a function to process the form.

The function `generateRandomString()` generates a random string of a specified length, which is used as a token for email verification.

The function `sendEmail()` sends an email to a specified recipient with a specified subject and message.

The function `processForm()` processes the form data, validates it, generates a random token, saves the token and the user's email address in the database, and sends an email with a link to the token.

The code also includes a HTML form that calls the `processForm()` function when the submit button is clicked.

When a user submits the form, the `processForm()` function is called, which validates the form data, generates a random token, saves the token and the user's email address in the database, and sends an email with a link to the token.

The user then needs to click on the link in the email to verify their email address.