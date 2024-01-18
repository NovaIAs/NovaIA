```php
<?php
// Including the required libraries
require_once 'vendor/autoload.php';
require_once 'config.php';

// Use Guzzle HTTP Client for making requests
use Guzzle\Http\Client;

// Use Monolog for logging
use Monolog\Logger;
use Monolog\Handler\StreamHandler;

// Use Carbon for date manipulation
use Carbon\Carbon;

// Use League\Csv for CSV handling
use League\Csv\Reader;

// Initialize logger
$logger = new Logger('script');
$logger->pushHandler(new StreamHandler('script.log', Logger::DEBUG));

// Define the endpoint URL
$endpointUrl = 'https://example.com/api/v1/';

// Define the CSV file path
$csvFilePath = 'path/to/data.csv';

// Define the batch size for sending data
$batchSize = 100;

// Initialize Guzzle Client
$client = new Client($endpointUrl);

// Initialize CSV Reader
$csvReader = Reader::createFromPath($csvFilePath);

// Read the CSV file in chunks
$csvReader->setDelimiter(';');
$csvReader->setHeaderOffset(0);
$csvData = [];

foreach ($csvReader->getRecords() as $record) {
    $csvData[] = $record;

    // Check if the batch size is reached
    if (count($csvData) >= $batchSize) {
        // Send the batch data to the endpoint using Guzzle
        $response = $client->post('data/batch', null, json_encode($csvData));

        // Log the response
        $logger->info(sprintf('Batch sent: %s', json_encode($csvData)));

        // Clear the batch data
        $csvData = [];
    }
}

// If there is any remaining data, send it
if (count($csvData) > 0) {
    $response = $client->post('data/batch', null, json_encode($csvData));

    // Log the response
    $logger->info(sprintf('Batch sent: %s', json_encode($csvData)));
}

// Log the completion of the script
$logger->info('Script completed successfully');
```

Explanation:

1. We include the required libraries and initialize the Guzzle HTTP Client, Monolog logger, Carbon date manipulation library, and League\Csv library.
2. We define the endpoint URL, CSV file path, batch size, and initialize the CSV Reader.
3. We iterate through the CSV file in chunks of the defined batch size.
4. In each iteration, we collect CSV records in the `$csvData` array.
5. When the batch size is reached, we send the collected data to the endpoint using the Guzzle Client and log the response.
6. We then clear the `$csvData` array to collect the next batch of data.
7. After processing all the data, we check if there is any remaining data and send it to the endpoint if necessary.
8. Finally, we log the completion of the script.