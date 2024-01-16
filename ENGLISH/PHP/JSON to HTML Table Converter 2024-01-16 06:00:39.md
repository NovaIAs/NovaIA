```php
$json_data = '{
    "employees": [
        {
            "id": 1,
            "name": "John Doe",
            "email": "johndoe@example.com",
            "phone": "555-123-4567",
            "department": "Sales"
        },
        {
            "id": 2,
            "name": "Jane Smith",
            "email": "janesmith@example.com",
            "phone": "555-234-5678",
            "department": "Marketing"
        },
        {
            "id": 3,
            "name": "Michael Jones",
            "email": "michaeljones@example.com",
            "phone": "555-345-6789",
            "department": "Engineering"
        },
        // ... more employees
    ]
}';

// Convert JSON string to PHP array
$employees = json_decode($json_data, true);

// Create a table header
echo "<table>";
echo "<tr>";
echo "<th>ID</th>";
echo "<th>Name</th>";
echo "<th>Email</th>";
echo "<th>Phone</th>";
echo "<th>Department</th>";
echo "</tr>";

// Iterate over the employees array and display each employee's data in a table row
foreach ($employees as $employee) {
    echo "<tr>";
    echo "<td>{$employee['id']}</td>";
    echo "<td>{$employee['name']}</td>";
    echo "<td>{$employee['email']}</td>";
    echo "<td>{$employee['phone']}</td>";
    echo "<td>{$employee['department']}</td>";
    echo "</tr>";
}

// Close the table
echo "</table>";
?>
```

This code is a PHP script that parses a JSON string containing employee data, converts it to a PHP array, and then displays the data in an HTML table.

Here is a breakdown of the code:

* The `$json_data` variable contains a JSON string representing employee data.
* The `json_decode()` function is used to convert the JSON string to a PHP array. The `true` argument specifies that the JSON data should be converted to an associative array, where the keys are the property names and the values are the property values.
* The `echo` statement is used to create the HTML table header.
* The `foreach` loop is used to iterate over the `$employees` array and display each employee's data in a table row.
* The `echo` statements within the loop are used to display the employee's ID, name, email, phone number, and department in separate table cells.
* The `echo` statement at the end of the script is used to close the HTML table.

When the script is executed, it will output an HTML table containing the employee data. The table will have five columns: ID, Name, Email, Phone, and Department. Each row in the table will represent an employee, and the data in each cell will be the corresponding employee's information.