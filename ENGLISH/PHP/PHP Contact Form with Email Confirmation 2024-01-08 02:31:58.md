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

// Define a function to process a form submission
function processForm() {
  // Get the form data
  $name = $_POST['name'];
  $email = $_POST['email'];
  $message = $_POST['message'];

  // Validate the form data
  if (empty($name) || empty($email) || empty($message)) {
    // Display an error message
    echo 'Please fill out all fields.';
    return;
  }

  // Generate a random string to use as a token
  $token = generateRandomString(32);

  // Save the form data to a database
  $query = 'INSERT INTO contact_form (name, email, message, token) VALUES (?, ?, ?, ?)';
  $stmt = $conn->prepare($query);
  $stmt->bind_param('ssss', $name, $email, $message, $token);
  $stmt->execute();

  // Send an email to the user with a link to confirm their submission
  $link = 'http://example.com/confirm.php?token=' . $token;
  $message = 'Thank you for your submission. Please click the following link to confirm your submission: ' . $link;
  sendEmail($email, 'Confirm your submission', $message);

  // Display a success message
  echo 'Your submission has been sent. Please check your email for a confirmation link.';
}

// Check if the form has been submitted
if (isset($_POST['submit'])) {
  processForm();
}
?>

<!-- HTML form -->
<form action="contact.php" method="post">
  <input type="text" name="name" placeholder="Your name">
  <input type="email" name="email" placeholder="Your email">
  <textarea name="message" placeholder="Your message"></textarea>
  <input type="submit" name="submit" value="Send">
</form>
```

Explanation:

* The `generateRandomString()` function generates a random string of a given length. It is used to generate a token that is used to confirm the user's submission.
* The `sendEmail()` function sends an email to a given recipient with a given subject and message.
* The `processForm()` function processes the form submission. It validates the form data, saves the data to a database, and sends an email to the user with a link to confirm their submission.
* The HTML form is used to collect the user's name, email, and message.
* When the form is submitted, the `processForm()` function is called to process the submission.