```
# Define a method to calculate the factorial of a number
def factorial(n)
  # If n is less than 0, return 1 (the factorial of 0)
  return 1 if n < 0

  # Initialize the factorial to 1
  factorial = 1

  # Iterate from 1 to n, multiplying the factorial by each number
  (1..n).each do |i|
    factorial *= i
  end

  # Return the factorial
  factorial
end

# Define a method to calculate the sum of an array of numbers
def sum(numbers)
  # Initialize the sum to 0
  sum = 0

  # Iterate over the array, adding each number to the sum
  numbers.each do |number|
    sum += number
  end

  # Return the sum
  sum
end

# Define a method to calculate the average of an array of numbers
def average(numbers)
  # If the array is empty, return 0
  return 0 if numbers.empty?

  # Calculate the sum of the numbers
  sum = sum(numbers)

  # Calculate the average by dividing the sum by the number of numbers
  average = sum / numbers.length

  # Return the average
  average
end

# Define a method to find the maximum value in an array of numbers
def max(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Initialize the maximum value to the first number in the array
  max_value = numbers[0]

  # Iterate over the array, comparing each number to the maximum value
  numbers.each do |number|
    if number > max_value
      max_value = number
    end
  end

  # Return the maximum value
  max_value
end

# Define a method to find the minimum value in an array of numbers
def min(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Initialize the minimum value to the first number in the array
  min_value = numbers[0]

  # Iterate over the array, comparing each number to the minimum value
  numbers.each do |number|
    if number < min_value
      min_value = number
    end
  end

  # Return the minimum value
  min_value
end

# Define a method to find the median value in an array of numbers
def median(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Sort the array in ascending order
  numbers.sort!

  # If the array has an even number of elements, the median is the average of the two middle elements
  if numbers.length.even?
    median = (numbers[numbers.length / 2 - 1] + numbers[numbers.length / 2]) / 2.0
  # If the array has an odd number of elements, the median is the middle element
  else
    median = numbers[numbers.length / 2]
  end

  # Return the median
  median
end

# Define a method to find the mode value in an array of numbers
def mode(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Create a hash to store the number of times each number appears in the array
  number_counts = Hash.new(0)
  numbers.each do |number|
    number_counts[number] += 1
  end

  # Find the number that appears the most times
  mode_number = nil
  max_count = 0
  number_counts.each do |number, count|
    if count > max_count
      mode_number = number
      max_count = count
    end
  end

  # Return the mode
  mode_number
end

# Define a method to find the range of an array of numbers
def range(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Find the maximum and minimum values in the array
  max_value = max(numbers)
  min_value = min(numbers)

  # Calculate the range by subtracting the minimum value from the maximum value
  range = max_value - min_value

  # Return the range
  range
end

# Define a method to find the variance of an array of numbers
def variance(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Calculate the mean of the numbers
  mean = average(numbers)

  # Calculate the sum of the squares of the differences between each number and the mean
  sum_of_squares = 0
  numbers.each do |number|
    sum_of_squares += (number - mean)**2
  end

  # Calculate the variance by dividing the sum of the squares by the number of numbers
  variance = sum_of_squares / numbers.length

  # Return the variance
  variance
end

# Define a method to find the standard deviation of an array of numbers
def standard_deviation(numbers)
  # If the array is empty, return nil
  return nil if numbers.empty?

  # Calculate the variance
  variance = variance(numbers)

  # Calculate the standard deviation by taking the square root of the variance
  standard_deviation = Math.sqrt(variance)

  # Return the standard deviation
  standard_deviation
end
```

Explanation:

This code defines a collection of methods for performing various statistical calculations on arrays of numbers. Let's break down what each method does:

1. `factorial(n)`: Calculates the factorial of a non-negative integer `n`.

2. `sum(numbers)`: Calculates the sum of an array of numbers.

3. `average(numbers)`: Calculates the average (mean) of an array of numbers.

4. `max(numbers)`: Finds the maximum value in an array of numbers.

5. `min(numbers)`: Finds the minimum value in an array of numbers.

6. `median(numbers)`: Finds the median value in an array of numbers.

7. `mode(numbers)`: Finds the mode (most frequently occurring value) in an array of numbers.

8. `range(numbers)`: Calculates the range (difference between the maximum and minimum values) of an array of numbers.

9. `variance(numbers)`: Calculates the variance (measure of how spread out the data is) of an array of numbers.

10. `standard_deviation(numbers)`: Calculates the standard deviation (square root of the variance) of an array of numbers.

All these methods take an array of numbers as input and return the desired statistical value. These methods provide a comprehensive set of tools for performing common statistical calculations on numerical data.