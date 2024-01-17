```php
<?php

// Define the main function
function main() {
    // Create a multidimensional array to store data
    $data = [
        'name' => 'John Doe',
        'age' => 30,
        'address' => [
            'street' => '123 Main Street',
            'city' => 'Anytown',
            'state' => 'CA',
            'zip' => '12345'
        ],
        'phoneNumbers' => [
            '(123) 456-7890',
            '(098) 765-4321'
        ],
        'hobbies' => [
            'reading',
            'hiking',
            'camping'
        ]
    ];

    // Convert the array to JSON format
    $json = json_encode($data);

    // Send the JSON data to the client
    header('Content-Type: application/json');
    echo $json;
}

// Execute the main function
main();
```

Explanation:

1. We define a multidimensional array `$data` to store various information about a person. It includes their name, age, address, phone numbers, and hobbies.

2. We use the `json_encode()` function to convert the `$data` array into a JSON string. JSON (JavaScript Object Notation) is a popular data format for representing objects and data structures in a lightweight and human-readable format.

3. We set the `Content-Type` header to `application/json` to inform the client that the response is in JSON format.

4. Finally, we echo out the JSON string to send it as the response to the client.

This code demonstrates how to convert a PHP array into a JSON string and send it as a response to the client in JSON format. This is useful for creating web APIs or sending data between different systems that use JSON as their data exchange format.