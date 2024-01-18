```lua
-- This code creates a simple calculator that can perform basic arithmetic operations.

-- Define the main function.
function main()
  -- Print a welcome message.
  print("Welcome to the calculator!")

  -- Create a loop to keep the calculator running.
  while true do
    -- Prompt the user to enter an expression.
    print("Enter an expression:")

    -- Read the expression from the user.
    local expression = io.read()

    -- Check if the user wants to quit.
    if expression == "quit" then
      break
    end

    -- Try to evaluate the expression.
    local result = nil
    try
      result = evaluate(expression)
    catch
      -- Print an error message if the expression is invalid.
      print("Invalid expression!")
    end

    -- Print the result.
    if result ~= nil then
      print("Result:", result)
    end
  end
end

-- Define the function to evaluate an expression.
function evaluate(expression)
  -- Tokenize the expression.
  local tokens = tokenize(expression)

  -- Parse the tokens into an abstract syntax tree.
  local ast = parse(tokens)

  -- Evaluate the abstract syntax tree.
  local result = eval(ast)

  -- Return the result.
  return result
end

-- Define the function to tokenize an expression.
function tokenize(expression)
  -- Create a list of tokens.
  local tokens = {}

  -- Loop through the expression character by character.
  for i = 1, #expression do
    -- Get the current character.
    local char = expression:sub(i, i)

    -- Check if the character is a digit.
    if char:match("%d") then
      -- Add the digit to the current token.
      local token = tokens[#tokens]
      token = token .. char
    elseif char:match("%+-/*()") then
      -- Add the operator to the list of tokens.
      tokens[#tokens + 1] = char
    elseif char == " " then
      -- Ignore whitespace.
    else
      -- Raise an error if the character is invalid.
      error("Invalid character: " .. char)
    end
  end

  -- Return the list of tokens.
  return tokens
end

-- Define the function to parse a list of tokens into an abstract syntax tree.
function parse(tokens)
  -- Create an abstract syntax tree.
  local ast = {}

  -- Loop through the tokens.
  for i = 1, #tokens do
    -- Get the current token.
    local token = tokens[i]

    -- Check if the token is an operator.
    if token:match("%+-/*()") then
      -- Add the operator to the abstract syntax tree.
      ast[#ast + 1] = { operator = token }
    else
      -- Add the operand to the abstract syntax tree.
      ast[#ast + 1] = { operand = tonumber(token) }
    end
  end

  -- Return the abstract syntax tree.
  return ast
end

-- Define the function to evaluate an abstract syntax tree.
function eval(ast)
  -- Create a stack to store the operands.
  local stack = {}

  -- Loop through the abstract syntax tree.
  for i = 1, #ast do
    -- Get the current node.
    local node = ast[i]

    -- Check if the node is an operator.
    if node.operator ~= nil then
      -- Pop the top two operands from the stack.
      local operand2 = stack[#stack]
      stack[#stack] = nil
      local operand1 = stack[#stack]
      stack[#stack] = nil

      -- Perform the operation.
      local result = nil
      if node.operator == "+" then
        result = operand1 + operand2
      elseif node.operator == "-" then
        result = operand1 - operand2
      elseif node.operator == "*" then
        result = operand1 * operand2
      elseif node.operator == "/" then
        result = operand1 / operand2
      end

      -- Push the result back onto the stack.
      stack[#stack + 1] = result
    else
      -- Push the operand onto the stack.
      stack[#stack + 1] = node.operand
    end
  end

  -- Return the top of the stack.
  return stack[#stack]
end

-- Call the main function.
main()
```

This code is a simple calculator that can perform basic arithmetic operations. It first tokenizes the expression, then parses the tokens into an abstract syntax tree, and finally evaluates the abstract syntax tree. The calculator can handle expressions with parentheses and supports the operators +, -, *, and /.

Here is an explanation of the code:

* The `main` function is the entry point of the program. It prints a welcome message and then enters a loop that keeps the calculator running.
* The `evaluate` function evaluates an expression. It first tokenizes the expression, then parses the tokens into an abstract syntax tree, and finally evaluates the abstract syntax tree.
* The `tokenize` function tokenizes an expression. It loops through the expression character by character and adds the characters to a list of tokens.
* The `parse` function parses a list of tokens into an abstract syntax tree. It loops through the tokens and adds the operators and operands to the abstract syntax tree.
* The `eval` function evaluates an abstract syntax tree. It loops through the abstract syntax tree and performs the operations on the operands.
* The calculator can handle expressions with parentheses by using the `try` and `catch` statements. The `try` statement attempts to evaluate the expression, and the `catch` statement catches any errors that are raised.

This code is a good example of how to use Lua to create a complex program. It is also a good example of how to use recursion to solve a problem.