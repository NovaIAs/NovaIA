```ruby
# This Ruby code recursively generates all possible binary strings of a given length.

# Define a method to generate binary strings of a given length.
def generate_binary_strings(length)
  # If the length is 0, return an empty array.
  if length == 0
    return [""]
  end

  # Otherwise, recursively generate binary strings of length - 1.
  shorter_strings = generate_binary_strings(length - 1)

  # For each shorter string, append a 0 and a 1 to create two longer strings.
  longer_strings = []
  shorter_strings.each do |string|
    longer_strings << string + "0"
    longer_strings << string + "1"
  end

  # Return the array of longer binary strings.
  return longer_strings
end

# Get the length of the binary strings to generate from the user.
puts "Enter the length of the binary strings to generate:"
length = gets.chomp.to_i

# Generate all possible binary strings of the given length.
binary_strings = generate_binary_strings(length)

# Print the binary strings to the console.
puts "All possible binary strings of length #{length}:"
binary_strings.each do |string|
  puts string
end
```

his code first defines a method called `generate_binary_strings` that takes a single argument, `length`, which represents the length of the binary strings to generate.

Inside the `generate_binary_strings` method, there is a base case: if the `length` is 0, the method simply returns an empty array. This is because there is only one binary string of length 0, which is the empty string.

If the `length` is not 0, the method recursively calls itself to generate all possible binary strings of length `length - 1`. This is because any binary string of length `length` can be constructed by appending a 0 or a 1 to a binary string of length `length - 1`.

After generating all possible binary strings of length `length - 1`, the method iterates over each of these strings and appends a 0 and a 1 to each string to create two longer strings. These two longer strings are then added to an array.

Finally, the method returns the array of longer binary strings.

The code then prompts the user to enter the length of the binary strings to generate, calls the `generate_binary_strings` method to generate all possible binary strings of that length, and prints the binary strings to the console.