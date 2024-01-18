```php
<?php

// Define the necessary constants
define('DB_HOST', 'localhost');
define('DB_USER', 'username');
define('DB_PASS', 'password');
define('DB_NAME', 'database_name');

// Connect to the database
$conn = new mysqli(DB_HOST, DB_USER, DB_PASS, DB_NAME);

// Check the connection
if($conn->connect_error){
    die("Connection failed: " . $conn->connect_error);
}

// Define the query
$query = "SELECT * FROM tbl_users WHERE user_type = 'admin'";

// Execute the query
$result = $conn->query($query);

// Check the result
if($result->num_rows > 0){
    // Display the results
    while($row = $result->fetch_assoc()){
        echo "ID: " . $row["user_id"] . "<br>";
        echo "Name: " . $row["user_name"] . "<br>";
        echo "Email: " . $row["user_email"] . "<br>";
        echo "Type: " . $row["user_type"] . "<br><br>";
    }
}else{
    // No results found
    echo "No admins found.";
}

// Close the connection
$conn->close();

?>
```

Explanation:

1. **Define the necessary constants**: This section defines the constants required for the database connection and query.

2. **Connect to the database**: The `mysqli` class is used to establish a connection to the database.

3. **Check the connection**: This step verifies if the connection was successful or not. If there's an error, it displays an error message and exits the script.

4. **Define the query**: The SQL query is defined to select all rows from the `tbl_users` table where the `user_type` column is equal to 'admin'.

5. **Execute the query**: The `query()` method is used to execute the query and store the result in the `$result` variable.

6. **Check the result**: This step checks if there are any rows returned by the query. If there are, it proceeds to display them.

7. **Display the results**: If there are results, a `while` loop is used to iterate through each row and display the user ID, name, email, and type.

8. **Close the connection**: Finally, the `close()` method is called to close the database connection.