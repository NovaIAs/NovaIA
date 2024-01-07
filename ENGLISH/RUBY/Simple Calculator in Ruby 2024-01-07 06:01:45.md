```ruby
# This is a Ruby program that simulates a simple calculator.

# Define a class called Calculator.
class Calculator
  # Define an initialize method for the Calculator class.
  # This method is called when a new Calculator object is created.
  def initialize()
    @result = 0
  end

  # Define an add method for the Calculator class.
  # This method takes two numbers as arguments and adds them together.
  def add(num1, num2)
    @result = num1 + num2
  end

  # Define a subtract method for the Calculator class.
  # This method takes two numbers as arguments and subtracts the second number from the first number.
  def subtract(num1, num2)
    @result = num1 - num2
  end

  # Define a multiply method for the Calculator class.
  # This method takes two numbers as arguments and multiplies them together.
  def multiply(num1, num2)
    @result = num1 * num2
  end

  # Define a divide method for the Calculator class.
  # This method takes two numbers as arguments and divides the first number by the second number.
  def divide(num1, num2)
    @result = num1 / num2
  end

  # Define a get_result method for the Calculator class.
  # This method returns the result of the last calculation that was performed.
  def get_result()
    return @result
  end
end

# Create a new Calculator object.
calculator = Calculator.new()

# Add two numbers together.
calculator.add(1, 2)

# Subtract one number from another.
calculator.subtract(5, 3)

# Multiply two numbers together.
calculator.multiply(2, 3)

# Divide one number by another.
calculator.divide(10, 2)

# Get the result of the last calculation that was performed.
result = calculator.get_result()

# Print the result to the console.
puts result
```

This code defines a class called Calculator that simulates a simple calculator. The Calculator class has an initialize method that is called when a new Calculator object is created. The initialize method initializes the @result instance variable to 0.

The Calculator class also has four methods: add(), subtract(), multiply(), and divide(). These methods take two numbers as arguments and perform the specified operation on the two numbers. The result of the operation is stored in the @result instance variable.

The Calculator class also has a get_result() method that returns the result of the last calculation that was performed.

The code creates a new Calculator object and then uses the add(), subtract(), multiply(), and divide() methods to perform a series of calculations. The result of each calculation is printed to the console.