```ruby
puts "Loading external libraries..."
require 'json'
require 'net/http'
require 'cgi'

puts "Defining constants..."
API_URL = 'https://api.example.com'
API_KEY = 'secret_api_key'
DEFAULT_LIMIT = 10

puts "Creating a reusable HTTP client..."
http_client = Net::HTTP.new(URI.parse(API_URL).host, URI.parse(API_URL).port)

puts "Defining the main method..."
def get_data(query, limit = DEFAULT_LIMIT)
  # Build the request URL with query parameters
  request_url = "#{API_URL}/search?q=#{CGI.escape(query)}&limit=#{limit}"

  # Create the request object
  request = Net::HTTP::Get.new(request_url)

  # Add the API key to the request headers
  request['Authorization'] = "Bearer #{API_KEY}"

  # Send the request and get the response
  response = http_client.request(request)

  # Check the response status code
  if response.code == '200'
    # Parse the JSON response
    data = JSON.parse(response.body)

    # Return the extracted data
    data['results']
  else
    # Handle non-200 responses appropriately
    raise "Error: HTTP status code #{response.code}"
  end
end

puts "Defining the main program..."
def main
  # Get the user's search query
  puts "Enter your search query:"
  query = gets.chomp

  # Get the data from the API and print it
  puts "Searching for \"#{query}\"..."
  data = get_data(query)
  data.each { |result| puts result['title'] }
end

puts "Running the main program..."
main
```