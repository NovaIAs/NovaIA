```ruby
# Create a hash to store the data
data = {}

# Get the user input
print "Enter the name: "
name = gets.chomp

print "Enter the age: "
age = gets.chomp.to_i

print "Enter the occupation: "
occupation = gets.chomp

# Add the data to the hash
data[:name] = name
data[:age] = age
data[:occupation] = occupation

# Print the data
puts "Name: #{data[:name]}"
puts "Age: #{data[:age]}"
puts "Occupation: #{data[:occupation]}"

# Create an array to store the data
data_array = []

# Add the data to the array
data_array << data

# Print the data
puts "Data array:"
puts data_array

# Create a method to calculate the average age
def calculate_average_age(data_array)
  # Check if the array is empty
  if data_array.empty?
    # Return nil if the array is empty
    return nil
  end

  # Calculate the sum of the ages
  sum_of_ages = 0
  data_array.each do |data|
    sum_of_ages += data[:age]
  end

  # Calculate the average age
  average_age = sum_of_ages / data_array.length

  # Return the average age
  return average_age
end

# Calculate the average age
average_age = calculate_average_age(data_array)

# Print the average age
puts "Average age: #{average_age}"
```

This code:

1. Creates a hash to store the user data.
2. Gets the user input for the name, age, and occupation.
3. Adds the data to the hash.
4. Prints the data.
5. Creates an array to store the data.
6. Adds the data to the array.
7. Prints the array.
8. Defines a method to calculate the average age.
9. Calculates the average age.
10. Prints the average age.