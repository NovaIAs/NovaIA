```ruby
# This is a program that simulates a simple calculator.

# Define the main method, which is the entry point of the program.
def main()
  # Create a loop that will continue until the user enters "q" to quit.
  while true
    # Prompt the user to enter an expression.
    print "Enter an expression (or 'q' to quit): "

    # Read the user's input.
    input = gets.chomp

    # Check if the user entered "q" to quit.
    if input == "q"
      break
    end

    # Parse the user's input into an array of tokens.
    tokens = input.split

    # Evaluate the expression and print the result.
    result = evaluate(tokens)
    puts "Result: #{result}"
  end
end

# Define the evaluate method, which evaluates an array of tokens and returns the result.
def evaluate(tokens)
  # Create a stack to store the operands.
  stack = []

  # Iterate over the tokens.
  tokens.each do |token|
    # Check if the token is an operator.
    if is_operator?(token)
      # Pop the top two operands from the stack.
      operand2 = stack.pop
      operand1 = stack.pop

      # Apply the operator to the operands.
      result = apply_operator(token, operand1, operand2)

      # Push the result back onto the stack.
      stack.push(result)
    else
      # Push the token onto the stack.
      stack.push(token.to_f)
    end
  end

  # Return the top of the stack, which is the result of the expression.
  return stack.pop
end

# Define the is_operator? method, which checks if a token is an operator.
def is_operator?(token)
  return ["+", "-", "*", "/"].include?(token)
end

# Define the apply_operator method, which applies an operator to two operands.
def apply_operator(operator, operand1, operand2)
  case operator
  when "+"
    return operand1 + operand2
  when "-"
    return operand1 - operand2
  when "*"
    return operand1 * operand2
  when "/"
    return operand1 / operand2
  end
end

# Call the main method to start the program.
main()
```

This program is a simple calculator that can evaluate expressions entered by the user. The program uses a stack to store the operands and operators, and it evaluates the expression by applying the operators to the operands in the correct order. The program supports the following operators: +, -, *, and /.

The program starts by prompting the user to enter an expression. The user can enter any valid expression, or they can enter "q" to quit the program. Once the user enters an expression, the program parses the expression into an array of tokens. The program then evaluates the expression by calling the `evaluate` method, which takes the array of tokens as its argument.

The `evaluate` method uses a stack to store the operands and operators. The method iterates over the tokens in the array, and for each token, it checks if it is an operator. If it is, the method pops the top two operands from the stack and applies the operator to them. The result of the operation is then pushed back onto the stack. If the token is not an operator, the method simply pushes it onto the stack.

After the method has iterated over all of the tokens in the array, the top of the stack contains the result of the expression. The method returns this result to the caller.

The `is_operator?` method checks if a token is an operator. The method returns `true` if the token is one of the following: +, -, *, or /. Otherwise, the method returns `false`.

The `apply_operator` method applies an operator to two operands. The method takes the operator and the two operands as its arguments. The method then applies the operator to the operands and returns the result of the operation.

The `main` method is the entry point of the program. The method starts by calling the `evaluate` method to evaluate the expression entered by the user. The method then prints the result of the expression to the console. The method then prompts the user to enter another expression, or to enter "q" to quit the program. The method continues to loop until the user enters "q".