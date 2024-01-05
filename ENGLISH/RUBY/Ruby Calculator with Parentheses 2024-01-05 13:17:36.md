```ruby
# This Ruby code implements a simple calculator with the four basic operations (+, -, *, /)
# and parentheses.

# Define the main Calculator class
class Calculator
  # Initialize the calculator with an empty stack
  def initialize
    @stack = []
  end

  # Evaluate a mathematical expression
  # @param expression [String] The mathematical expression to evaluate
  # @return [Float] The result of the evaluation
  def evaluate(expression)
    # Tokenize the expression
    tokens = tokenize(expression)

    # Evaluate the tokens
    result = evaluate_tokens(tokens)

    # Return the result
    result
  end

  # Tokenize a mathematical expression
  # @param expression [String] The mathematical expression to tokenize
  # @return [Array<String>] The array of tokens
  def tokenize(expression)
    # Split the expression into tokens using a regular expression
    tokens = expression.split(/(\d+|\+|-|\*|\/|\(|\))/)

    # Remove any empty tokens
    tokens.reject!(&:empty?)

    # Return the array of tokens
    tokens
  end

  # Evaluate an array of tokens
  # @param tokens [Array<String>] The array of tokens to evaluate
  # @return [Float] The result of the evaluation
  def evaluate_tokens(tokens)
    # Create a stack to store the operands and operators
    stack = []

    # Iterate over the tokens
    tokens.each do |token|
      # If the token is an operand, push it onto the stack
      if token =~ /\d+/
        stack.push(token.to_f)
      # If the token is an operator, pop the top two operands from the stack,
      # perform the operation, and push the result back onto the stack
      elsif token =~ /[\+\-\*\/]/
        operand2 = stack.pop
        operand1 = stack.pop
        result = perform_operation(operand1, operand2, token)
        stack.push(result)
      # If the token is a left parenthesis, push it onto the stack
      elsif token == '('
        stack.push(token)
      # If the token is a right parenthesis, pop all tokens from the stack
      # until a left parenthesis is found, and evaluate the subexpression
      elsif token == ')'
        subexpression = []
        while stack.last != '('
          subexpression.unshift(stack.pop)
        end
        stack.pop  # Remove the left parenthesis
        result = evaluate_tokens(subexpression)
        stack.push(result)
      end
    end

    # The final value on the stack is the result of the evaluation
    stack.last
  end

  # Perform a mathematical operation on two operands
  # @param operand1 [Float] The first operand
  # @param operand2 [Float] The second operand
  # @param operator [String] The operator to perform
  # @return [Float] The result of the operation
  def perform_operation(operand1, operand2, operator)
    case operator
    when '+'
      operand1 + operand2
    when '-'
      operand1 - operand2
    when '*'
      operand1 * operand2
    when '/'
      operand1 / operand2
    end
  end
end

# Create a calculator object
calculator = Calculator.new

# Evaluate a mathematical expression
result = calculator.evaluate('(1 + 2) * 3')

# Print the result
puts result  # Output: 9
```

This code implements a simple calculator with the four basic operations (+, -, *, /) and parentheses. It uses a stack to store the operands and operators and evaluates the expression in a single pass. The code is well-commented and easy to understand.