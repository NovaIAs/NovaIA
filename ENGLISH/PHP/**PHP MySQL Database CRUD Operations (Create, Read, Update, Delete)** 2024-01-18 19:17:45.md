```php
<?php

// Define the database connection parameters
$servername = "localhost";
$username = "root";
$password = "";
$dbname = "myDB";

// Create the database connection
$conn = new mysqli($servername, $username, $password, $dbname);

// Check the connection
if ($conn->connect_error) {
    die("Connection failed: " . $conn->connect_error);
}

// Define the SQL query to create the table
$sql = "CREATE TABLE IF NOT EXISTS users (
    id INT AUTO_INCREMENT PRIMARY KEY,
    name VARCHAR(255) NOT NULL,
    email VARCHAR(255) NOT NULL UNIQUE,
    password VARCHAR(255) NOT NULL,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP,
    updated_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP ON UPDATE CURRENT_TIMESTAMP
)";

// Execute the SQL query
if ($conn->query($sql) === TRUE) {
    echo "Table created successfully";
} else {
    echo "Error creating table: " . $conn->error;
}

// Define the SQL query to insert data into the table
$sql = "INSERT INTO users (name, email, password) VALUES (
    'John Doe',
    'john.doe@example.com',
    'password'
), (
    'Jane Doe',
    'jane.doe@example.com',
    'password'
), (
    'Peter Smith',
    'peter.smith@example.com',
    'password'
)";

// Execute the SQL query
if ($conn->query($sql) === TRUE) {
    echo "Data inserted successfully";
} else {
    echo "Error inserting data: " . $conn->error;
}

// Define the SQL query to select data from the table
$sql = "SELECT * FROM users";

// Execute the SQL query
$result = $conn->query($sql);

// Check if the query returned any results
if ($result->num_rows > 0) {
    // Output the results
    while ($row = $result->fetch_assoc()) {
        echo "ID: " . $row["id"] . " - Name: " . $row["name"] . " - Email: " . $row["email"] . "<br>";
    }
} else {
    echo "No results found";
}

// Define the SQL query to update data in the table
$sql = "UPDATE users SET name = 'John Smith' WHERE id = 1";

// Execute the SQL query
if ($conn->query($sql) === TRUE) {
    echo "Data updated successfully";
} else {
    echo "Error updating data: " . $conn->error;
}

// Define the SQL query to delete data from the table
$sql = "DELETE FROM users WHERE id = 3";

// Execute the SQL query
if ($conn->query($sql) === TRUE) {
    echo "Data deleted successfully";
} else {
    echo "Error deleting data: " . $conn->error;
}

// Close the database connection
$conn->close();

?>
```

Explanation:

1. We start by defining the database connection parameters:

   - `$servername`: The hostname of the database server.
   - `$username`: The username to connect to the database.
   - `$password`: The password to connect to the database.
   - `$dbname`: The name of the database to connect to.

2. We then create a new `mysqli` object and pass the connection parameters to it. This object will be used to connect to the database and execute SQL queries.

3. We check if the database connection was successful. If it was, we proceed to create the `users` table.

4. We define the SQL query to create the `users` table. The table will have the following columns:

   - `id`: The primary key of the table.
   - `name`: The name of the user.
   - `email`: The email address of the user.
   - `password`: The password of the user.
   - `created_at`: The timestamp when the user was created.
   - `updated_at`: The timestamp when the user was last updated.

5. We execute the SQL query to create the table. If the table was created successfully, we display a success message. Otherwise, we display an error message.

6. We define the SQL query to insert data into the `users` table. We insert three rows of data into the table.

7. We execute the SQL query to insert data into the table. If the data was inserted successfully, we display a success message. Otherwise, we display an error message.

8. We define the SQL query to select data from the `users` table. The query will select all the columns from all the rows in the table.

9. We execute the SQL query to select data from the table. If the query returned any results, we iterate over the results and display the data from each row. Otherwise, we display a message indicating that no results were found.

10. We define the SQL query to update data in the `users` table. The query will update the name of the user with the id of 1 to "John Smith".

11. We execute the SQL query to update data in the table. If the data was updated successfully, we display a success message. Otherwise, we display an error message.

12. We define the SQL query to delete data from the `users` table. The query will delete the user with the id of 3.

13. We execute the SQL query to delete data from the table. If the data was deleted successfully, we display a success message. Otherwise, we display an error message.

14. Finally, we close the database connection.