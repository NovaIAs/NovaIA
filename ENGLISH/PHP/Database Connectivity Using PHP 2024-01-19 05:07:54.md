```php
<?php

// Define the base URL for the application.
$base_url = 'http://example.com/';

// Define the database connection parameters.
$db_host = 'localhost';
$db_user = 'username';
$db_pass = 'password';
$db_name = 'database_name';

// Connect to the database.
$db = new mysqli($db_host, $db_user, $db_pass, $db_name);

// Check if the connection was successful.
if ($db->connect_error) {
  die('Could not connect to the database: ' . $db->connect_error);
}

// Define the query to get all the users from the database.
$query = 'SELECT * FROM users';

// Execute the query.
$result = $db->query($query);

// Check if the query was successful.
if (!$result) {
  die('Could not execute the query: ' . $db->error);
}

// Define an array to store the users.
$users = array();

// Fetch all the users from the result set.
while ($row = $result->fetch_assoc()) {
  $users[] = $row;
}

// Close the database connection.
$db->close();

// Define the header for the HTML document.
$header = '<!DOCTYPE html><html><head><title>Example Page</title></head><body>';

// Define the footer for the HTML document.
$footer = '</body></html>';

// Define the content for the HTML document.
$content = '<h1>Example Page</h1><ul>';

// Add the users to the content.
foreach ($users as $user) {
  $content .= '<li>' . $user['name'] . '</li>';
}

$content .= '</ul>';

// Define the complete HTML document.
$html = $header . $content . $footer;

// Output the HTML document.
echo $html;

?>
```

This code is a complete PHP script that connects to a MySQL database, retrieves all the users from the database, and then displays them in an HTML document. The code is divided into several sections, each of which is responsible for a specific task.

The first section of the code defines the base URL for the application and the database connection parameters. The second section connects to the database and checks if the connection was successful. The third section defines the query to get all the users from the database and executes the query. The fourth section defines an array to store the users and fetches all the users from the result set. The fifth section closes the database connection.

The sixth section defines the header and footer for the HTML document. The seventh section defines the content for the HTML document, which includes a list of the users. The eighth section defines the complete HTML document, which includes the header, content, and footer. The final section outputs the HTML document to the user.

This code is a good example of how to use PHP to connect to a database, retrieve data from the database, and then display the data in an HTML document. The code is also a good example of how to structure a PHP script into multiple sections, each of which is responsible for a specific task.