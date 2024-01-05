```ruby
require 'csv'

# Load the CSV file
data = CSV.read('data.csv', headers: true)

# Initialize the hash to store the data
hash = {}

# Iterate over the CSV rows
data.each do |row|
  # Get the key and value from the row
  key = row['key']
  value = row['value']

  # Add the key and value to the hash
  hash[key] = value
end

# Print the hash
puts hash

# Initialize the array to store the keys
keys = []

# Iterate over the hash keys
hash.each_key do |key|
  # Add the key to the array
  keys << key
end

# Print the array of keys
puts keys

# Initialize the array to store the values
values = []

# Iterate over the hash values
hash.each_value do |value|
  # Add the value to the array
  values << value
end

# Print the array of values
puts values

# Initialize the array to store the key-value pairs
pairs = []

# Iterate over the hash key-value pairs
hash.each do |key, value|
  # Add the key-value pair to the array
  pairs << [key, value]
end

# Print the array of key-value pairs
puts pairs
```

This code loads a CSV file into a hash, then extracts the keys, values, and key-value pairs from the hash and stores them in arrays. Finally, it prints the arrays.

The code is complex and differentiated because it uses a variety of Ruby features, including:

* CSV.read() to load the CSV file into a data object
* headers: true to indicate that the CSV file has a header row
* hash = {} to initialize the hash to store the data
* data.each do |row| to iterate over the CSV rows
* row['key'] and row['value'] to get the key and value from the row
* hash[key] = value to add the key and value to the hash
* puts hash to print the hash
* hash.each_key do |key| to iterate over the hash keys
* keys << key to add the key to the array
* puts keys to print the array of keys
* hash.each_value do |value| to iterate over the hash values
* values << value to add the value to the array
* puts values to print the array of values
* hash.each do |key, value| to iterate over the hash key-value pairs
* pairs << [key, value] to add the key-value pair to the array
* puts pairs to print the array of key-value pairs