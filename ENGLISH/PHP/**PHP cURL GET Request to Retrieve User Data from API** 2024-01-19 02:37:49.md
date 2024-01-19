```php
<?php

// Define the base URL for the API
$base_url = 'https://api.example.com/v1/';

// Initialize cURL
$curl = curl_init();

// Set cURL options
curl_setopt_array($curl, [
    CURLOPT_URL => $base_url . 'users',
    CURLOPT_RETURNTRANSFER => true,
    CURLOPT_ENCODING => '',
    CURLOPT_MAXREDIRS => 10,
    CURLOPT_TIMEOUT => 0,
    CURLOPT_FOLLOWLOCATION => true,
    CURLOPT_HTTP_VERSION => CURL_HTTP_VERSION_1_1,
    CURLOPT_CUSTOMREQUEST => 'GET',
]);

// Execute the cURL request
$response = curl_exec($curl);

// Close the cURL connection
curl_close($curl);

// Decode the JSON response
$data = json_decode($response, true);

// Check if there are any errors
if (isset($data['error'])) {
    echo 'Error: ' . $data['error'];
    exit;
}

// Loop through the users
foreach ($data['users'] as $user) {
    // Print the user's name and email
    echo 'Name: ' . $user['name'] . ' Email: ' . $user['email'] . PHP_EOL;
}

?>
```

**Explanation:**

This PHP code demonstrates a complex and differentiated use of cURL to make a GET request to an API and retrieve user data. Here's a breakdown of the code:

1. We define the base URL for the API in the `$base_url` variable.

2. We initialize cURL using the `curl_init()` function and store the result in the `$curl` variable.

3. We set various cURL options using `curl_setopt_array()`. This includes setting the URL to fetch (`CURLOPT_URL`), enabling the return of the response body (`CURLOPT_RETURNTRANSFER`), specifying the request method (`CURLOPT_CUSTOMREQUEST`), and more.

4. We execute the cURL request using `curl_exec($curl)`, which sends the request to the API and returns the response.

5. We close the cURL connection using `curl_close($curl)` to free up system resources.

6. We decode the JSON response using `json_decode()` to convert it into an associative array stored in the `$data` variable.

7. We check if there's an `error` key in the `$data` array. If there is, we display the error message and exit the script.

8. We use a `foreach` loop to iterate through the `users` array within the `$data` array.

9. For each user, we print their name and email using `echo`.

This code provides a comprehensive example of making an API call using cURL in PHP, validating the response, and iterating over the retrieved data.