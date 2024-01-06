```ruby
# This is a Ruby program that implements a simple calculator.

# Define a class called Calculator.
class Calculator
  # Define an initialize method to initialize the calculator.
  def initialize()
    # Create an empty array to store the operands.
    @operands = []

    # Create an empty array to store the operators.
    @operators = []
  end

  # Define a method called add to add two numbers.
  def add(a, b)
    # Add the two numbers together and return the result.
    a + b
  end

  # Define a method called subtract to subtract two numbers.
  def subtract(a, b)
    # Subtract the second number from the first number and return the result.
    a - b
  end

  # Define a method called multiply to multiply two numbers.
  def multiply(a, b)
    # Multiply the two numbers together and return the result.
    a * b
  end

  # Define a method called divide to divide two numbers.
  def divide(a, b)
    # Divide the first number by the second number and return the result.
    a / b
  end

  # Define a method called evaluate to evaluate a mathematical expression.
  def evaluate(expression)
    # Split the expression into an array of tokens.
    tokens = expression.split(" ")

    # Iterate over the tokens.
    tokens.each do |token|
      # If the token is an operand, add it to the operands array.
      if token =~ /\d+/
        @operands << token.to_i
      # If the token is an operator, add it to the operators array.
      elsif token =~ /[+\-*/]/
        @operators << token
      end
    end

    # Evaluate the expression.
    while @operators.length > 0
      # Get the first operator and the first two operands.
      operator = @operators.shift
      operand1 = @operands.shift
      operand2 = @operands.shift

      # Evaluate the operator and store the result in the operands array.
      @operands << send(operator, operand1, operand2)
    end

    # Return the result of the expression.
    @operands[0]
  end
end

# Create a new calculator object.
calculator = Calculator.new

# Evaluate a mathematical expression.
result = calculator.evaluate("2 + 3 * 4 - 5")

# Print the result.
puts result
```

This code defines a class called Calculator that implements a simple calculator. The class has three methods: add, subtract, multiply, and divide, which are used to perform the basic arithmetic operations. The class also has a method called evaluate, which is used to evaluate a mathematical expression. The evaluate method splits the expression into an array of tokens, then iterates over the tokens and adds operands and operators to the operands and operators arrays, respectively. The method then evaluates the expression by popping the first operator and the first two operands from the arrays, evaluating the operator, and storing the result in the operands array. The method repeats this process until there are no more operators in the operators array, and then returns the result of the expression.

The code then creates a new calculator object and uses the evaluate method to evaluate the expression "2 + 3 * 4 - 5". The result of the expression is then printed to the console.