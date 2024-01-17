```ruby
require 'json'
require 'net/http'
require 'uri'

# Define the URL of the API endpoint
api_url = 'https://api.example.com/v1/data'

# Define the request headers
headers = {
  'Content-Type' => 'application/json',
  'Authorization' => 'Bearer YOUR_API_KEY'
}

# Define the request body
request_body = {
  'field1' => 'value1',
  'field2' => 'value2',
  'field3' => 'value3'
}

# Create the HTTP request object
request = Net::HTTP::Post.new(URI.parse(api_url), headers)

# Set the request body
request.body = JSON.dump(request_body)

# Send the request to the API endpoint
response = Net::HTTP.start(URI.parse(api_url).host, URI.parse(api_url).port, use_ssl: true) do |http|
  http.request(request)
end

# Parse the response body as JSON
response_body = JSON.parse(response.body)

# Print the response
puts response_body
```

This code demonstrates a more complex use case with HTTP requests, JSON parsing, and error handling. It sends a POST request to an API endpoint, passing along JSON data in the request body, and handles the response by parsing it as JSON and printing it.

Let's break down the code step by step:

1. We start by requiring necessary modules and libraries:

   - 'json': For parsing and generating JSON data.
   - 'net/http': For making HTTP requests.
   - 'uri': For parsing and manipulating URIs.

2. We define the URL of the API endpoint (`api_url`) as a string.

3. We define the request headers (`headers`) as a hash, including the Content-Type and Authorization headers. You would need to replace `YOUR_API_KEY` with your actual API key.

4. We define the request body (`request_body`) as a hash containing the data to be sent in the POST request.

5. We create the HTTP request object (`request`) by specifying the HTTP method (POST), the URI of the API endpoint, and the headers.

6. We set the request body by converting the `request_body` hash to JSON using `JSON.dump`.

7. We send the request to the API endpoint using `Net::HTTP.start`. This establishes a connection to the API server and sends the request.

8. We handle the response by parsing the response body as JSON using `JSON.parse`.

9. Finally, we print the parsed response body, which should contain the API's response to the request.

This code demonstrates a more complex scenario involving HTTP requests, JSON data handling, and response parsing, which can be useful for interacting with APIs and web services.