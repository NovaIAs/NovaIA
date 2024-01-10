```php
<?php

// Define the base URL for the API
$base_url = 'https://api.example.com/v1/';

// Initialize an empty array to store the results
$results = [];

// Loop through the pages of results
$page = 1;
while (true) {
    // Make a GET request to the API to retrieve the current page of results
    $response = @file_get_contents($base_url . 'users?page=' . $page);
    if ($response === false) {
        // If the request failed, break out of the loop
        break;
    }

    // Decode the JSON response into an associative array
    $data = json_decode($response, true);

    // If the data is empty, break out of the loop
    if (empty($data)) {
        break;
    }

    // Add the results from the current page to the $results array
    $results = array_merge($results, $data['users']);

    // Increment the page number to get the next page of results
    $page++;
}

// Print the results
print_r($results);

```

This code is a complex and differentiated PHP script that demonstrates:

1. **Making a GET Request to an API**: It makes a GET request to an API endpoint (`$base_url . 'users?page=' . $page`) to retrieve data.

2. **Pagination Handling**: It handles pagination by iterating through multiple pages of results. It starts from page 1 and continues making requests until there are no more pages of results.

3. **JSON Decoding**: It decodes the JSON response from the API into an associative array using `json_decode()` to make the data accessible in PHP.

4. **Data Accumulation**: It accumulates the results from each page of the API response into a single `$results` array.

5. **Printing the Results**: Finally, it prints the accumulated `$results` array using `print_r()`.

This code demonstrates a comprehensive and practical approach to handling pagination and retrieving data from an API in PHP.