```ruby
# Import the necessary libraries.
require 'json'
require 'net/http'
require 'uri'

# Define the URL of the API endpoint.
api_url = 'https://example.com/api/v1/data'

# Define the HTTP request headers.
headers = {
  'Content-Type' => 'application/json',
  'Accept' => 'application/json'
}

# Define the request body.
request_body = {
  'name' => 'John Doe',
  'email' => 'johndoe@example.com',
  'password' => 'password'
}

# Create a new HTTP client.
http_client = Net::HTTP.new(URI.parse(api_url).host, URI.parse(api_url).port)

# Set the request method and headers.
http_client.use_ssl = true
http_client.verify_mode = OpenSSL::SSL::VERIFY_NONE
http_client.request_get(URI.parse(api_url).path, headers)

# Send the request and receive the response.
response = http_client.request_post(URI.parse(api_url).path, request_body.to_json, headers)

# Parse the response body as JSON.
response_body = JSON.parse(response.body)

# Print the response body.
puts response_body
```

This code demonstrates how to make an HTTP POST request to an API endpoint using Ruby. Here's a breakdown of the code:

1. **Library Imports**: It imports the necessary libraries for making HTTP requests, parsing JSON data, and handling SSL connections.

2. **API Endpoint URL**: The URL of the API endpoint to which the request will be made is defined.

3. **HTTP Request Headers**: The HTTP request headers are defined. These headers specify the content type and the accepted content type.

4. **Request Body**: The request body is defined as a hash containing the data to be sent to the API endpoint. In this case, it contains information about a user (name, email, and password).

5. **HTTP Client**: A new HTTP client is created. This client will be used to send the HTTP request.

6. **SSL Configuration**: The code sets the SSL configuration for the HTTP client. It disables SSL certificate verification to allow connections to insecure endpoints.

7. **HTTP GET Request**: An HTTP GET request is made to the API endpoint to retrieve the data.

8. **HTTP POST Request**: An HTTP POST request is made to the API endpoint to send the data contained in the request body.

9. **Response Handling**: The response from the API endpoint is received and parsed as JSON data.

10. **Printing the Response**: Finally, the parsed JSON response body is printed to the console.