```ruby
# Define a class called "Calculator"
class Calculator
  # Define an initialize method to initialize the calculator
  def initialize
    # Initialize the calculator's display to 0
    @display = 0
  end

  # Define an add method to add a number to the calculator's display
  def add(number)
    # Add the number to the calculator's display
    @display += number
  end

  # Define a subtract method to subtract a number from the calculator's display
  def subtract(number)
    # Subtract the number from the calculator's display
    @display -= number
  end

  # Define a multiply method to multiply the calculator's display by a number
  def multiply(number)
    # Multiply the calculator's display by the number
    @display *= number
  end

  # Define a divide method to divide the calculator's display by a number
  def divide(number)
    # Divide the calculator's display by the number
    @display /= number
  end

  # Define a clear method to clear the calculator's display
  def clear
    # Set the calculator's display to 0
    @display = 0
  end

  # Define a display method to return the calculator's display
  def display
    # Return the calculator's display
    @display
  end
end

# Create a new calculator object
calculator = Calculator.new

# Add 5 to the calculator's display
calculator.add(5)

# Subtract 2 from the calculator's display
calculator.subtract(2)

# Multiply the calculator's display by 3
calculator.multiply(3)

# Divide the calculator's display by 4
calculator.divide(4)

# Clear the calculator's display
calculator.clear

# Display the calculator's display
puts calculator.display
```

This code defines a class called "Calculator" that can perform basic arithmetic operations. The class has an initialize method that initializes the calculator's display to 0. It also has methods for adding, subtracting, multiplying, dividing, and clearing the display.

To use the calculator, you can create a new calculator object and then use the methods to perform calculations. For example, the following code creates a new calculator object, adds 5 to the display, subtracts 2 from the display, multiplies the display by 3, divides the display by 4, clears the display, and then displays the display:

```ruby
calculator = Calculator.new

calculator.add(5)
calculator.subtract(2)
calculator.multiply(3)
calculator.divide(4)
calculator.clear

puts calculator.display
```

This code will output the number 0, since the display was cleared before it was displayed.