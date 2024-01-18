```ruby
# This Ruby program creates a custom class called "ArrayExtender"
# that provides additional methods to the built-in Ruby Array class.

# Define the ArrayExtender class
class ArrayExtender
  # Define an instance method called "sum" that calculates the sum of all the
  # elements in the array.
  def sum
    total = 0  # Initialize a variable to store the sum
    self.each do |element|  # Iterate over each element in the array
      total += element  # Add the element to the total
    end
    total  # Return the total sum
  end

  # Define an instance method called "average" that calculates the average of all
  # the elements in the array.
  def average
    sum = self.sum  # Calculate the sum of all the elements
    length = self.length  # Get the length of the array
    average = sum / length  # Calculate the average
    average  # Return the average
  end

  # Define an instance method called "max_by" that returns the element in the
  # array with the highest value based on a given block.
  def max_by(&block)  # The &block parameter allows us to pass a block to the method
    max_element = self[0]  # Initialize the max_element with the first element
    max_value = block.call(max_element)  # Calculate the value of the max_element using the block

    self.each do |element|  # Iterate over each element in the array
      value = block.call(element)  # Calculate the value of the current element using the block
      if value > max_value  # Check if the current value is greater than the max_value
        max_element = element  # Update the max_element with the current element
        max_value = value  # Update the max_value with the current value
      end
    end

    max_element  # Return the element with the highest value
  end

  # Define an instance method called "min_by" that returns the element in the
  # array with the lowest value based on a given block.
  def min_by(&block)  # The &block parameter allows us to pass a block to the method
    min_element = self[0]  # Initialize the min_element with the first element
    min_value = block.call(min_element)  # Calculate the value of the min_element using the block

    self.each do |element|  # Iterate over each element in the array
      value = block.call(element)  # Calculate the value of the current element using the block
      if value < min_value  # Check if the current value is less than the min_value
        min_element = element  # Update the min_element with the current element
        min_value = value  # Update the min_value with the current value
      end
    end

    min_element  # Return the element with the lowest value
  end
end

# Create a new instance of the ArrayExtender class
array = ArrayExtender.new([1, 2, 3, 4, 5])

# Use the "sum" method to calculate the sum of the elements in the array
puts "Sum: #{array.sum}"  # Output: Sum: 15

# Use the "average" method to calculate the average of the elements in the array
puts "Average: #{array.average}"  # Output: Average: 3.0

# Use the "max_by" method to find the maximum element in the array based
# on the value of each element squared
max_by_square = array.max_by { |x| x**2 }  # Use a block to calculate the square of each element
puts "Maximum by square: #{max_by_square}"  # Output: Maximum by square: 5

# Use the "min_by" method to find the minimum element in the array based
# on the value of each element cubed
min_by_cube = array.min_by { |x| x**3 }  # Use a block to calculate the cube of each element
puts "Minimum by cube: #{min_by_cube}"  # Output: Minimum by cube: 1
```

This Ruby program provides a custom class called "ArrayExtender" that extends the built-in Ruby Array class with additional methods for calculating the sum, average, maximum, and minimum values. It also allows you to specify custom criteria for determining the maximum and minimum values using blocks.