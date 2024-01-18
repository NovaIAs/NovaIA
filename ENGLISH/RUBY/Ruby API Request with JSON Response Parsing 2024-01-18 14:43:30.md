```ruby
# Import the necessary libraries.
require 'json'
require 'net/http'
require 'uri'

# Define the base URL for the API.
base_url = 'https://api.example.com'

# Define the API key.
api_key = 'YOUR_API_KEY'

# Define the endpoint for getting a list of products.
products_endpoint = '/products'

# Create a new HTTP client.
http_client = Net::HTTP.new(base_url, 443)

# Set the HTTP client to use SSL.
http_client.use_ssl = true

# Create a new HTTP request object.
request = Net::HTTP::Get.new(products_endpoint)

# Add the API key to the request headers.
request['Authorization'] = "Bearer #{api_key}"

# Send the request to the API.
response = http_client.request(request)

# Check the response status code.
if response.code == '200'
  # Parse the response body as JSON.
  products = JSON.parse(response.body)

  # Print the list of products.
  products.each do |product|
    puts "Product ID: #{product['id']}"
    puts "Product Name: #{product['name']}"
    puts "Product Description: #{product['description']}"
    puts "Product Price: #{product['price']}"
  end
else
  # An error occurred. Print the error message.
  puts "An error occurred: #{response.body}"
end
```

This code demonstrates a more complex HTTP request-response interaction with an API using the Ruby programming language. It includes:

1. Importing the necessary Ruby libraries for making HTTP requests and parsing JSON responses.

2. Defining the base URL and API key for the REST API being used.

3. Defining the endpoint for getting a list of products from the API.

4. Creating a new HTTP client and configuring it to use SSL.

5. Creating a new HTTP GET request object and setting the appropriate headers, including the Authorization header with the API key.

6. Sending the request to the API and receiving the response.

7. Checking the response status code to see if the request was successful (HTTP 200).

8. Parsing the response body as JSON to extract the list of products.

9. Iterating over the list of products and printing their details, such as ID, name, description, and price.

10. Handling the case where an error occurs during the request-response process and printing the error message.

This code is more complex and demonstrates more advanced usage of Ruby and HTTP for interacting with an API. It includes error handling, JSON parsing, and structured output of the response data.