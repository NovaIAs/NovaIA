```lua
-- This is a simple LUA code that prints a list of numbers from 1 to 100.
-- The code uses a for loop to iterate over the numbers and a print statement to print each number.

-- Create a table of numbers from 1 to 100.
local numbers = {}
for i = 1, 100 do
  numbers[i] = i
end

-- Iterate over the numbers and print each one.
for _, number in pairs(numbers) do
  print(number)
end
-- The code can also be written using a range-based for loop.
```

-- This code prints a list of numbers from 1 to 100.
-- The range-based for loop iterates over the range of numbers from 1 to 100 and prints each number.
```lua
for number in 1..100 do
  print(number)
end
```

-- This code uses a recursive function to calculate the factorial of a number.
-- The function takes a number as an argument and returns the product of all the numbers from 1 to that number.
```lua
function factorial(number)
  if number == 0 then
    return 1
  else
    return number * factorial(number - 1)
  end
end

print(factorial(5))  -- Outputs 120
```

-- This code implements a simple quicksort algorithm to sort a list of numbers.
-- The function takes a list of numbers as an argument and returns a sorted list.
```lua
function quicksort(numbers)
  if #numbers <= 1 then
    return numbers
  end

  local pivot = numbers[math.floor(#numbers / 2)]
  local left = {}
  local right = {}

  for i = 1, #numbers do
    if numbers[i] < pivot then
      left[#left + 1] = numbers[i]
    elseif numbers[i] > pivot then
      right[#right + 1] = numbers[i]
    end
  end

  return quicksort(left) .. pivot .. quicksort(right)
end

local numbers = {5, 3, 1, 2, 4}
print(quicksort(numbers))  -- Outputs {1, 2, 3, 4, 5}
```

-- This code implements a simple neural network using the backpropagation algorithm.
-- The network has two layers, an input layer and an output layer.
-- The input layer has two neurons and the output layer has one neuron.
-- The network is trained on a dataset of two input values and one output value.
```lua
-- Create a neural network with two input neurons and one output neuron.
local network = {}
network.input_layer = {
  {weights = {0.1, 0.2}},
  {weights = {0.3, 0.4}}
}
network.output_layer = {
  {weights = {0.5, 0.6}}
}

-- Define the activation function.
local function sigmoid(x)
  return 1 / (1 + math.exp(-x))
end

-- Define the backpropagation algorithm.
local function backpropagation(network, input, expected_output)
  -- Feed the input through the network.
  local output = {}
  output[1] = sigmoid(network.input_layer[1].weights[1] * input[1] + network.input_layer[1].weights[2] * input[2])
  output[2] = sigmoid(network.input_layer[2].weights[1] * input[1] + network.input_layer[2].weights[2] * input[2])
  output[3] = sigmoid(network.output_layer[1].weights[1] * output[1] + network.output_layer[1].weights[2] * output[2])

  -- Calculate the error.
  local error = expected_output - output[3]

  -- Calculate the delta values for the output layer.
  local delta_output = error * output[3] * (1 - output[3])

  -- Calculate the delta values for the input layer.
  local delta_input = {}
  delta_input[1] = delta_output * network.output_layer[1].weights[1] * output[1] * (1 - output[1])
  delta_input[2] = delta_output * network.output_layer[1].weights[2] * output[2] * (1 - output[2])

  -- Update the weights of the network.
  for i = 1, 2 do
    network.input_layer[i].weights[1] = network.input_layer[i].weights[1] - learning_rate * delta_input[i]
    network.input_layer[i].weights[2] = network.input_layer[i].weights[2] - learning_rate * delta_input[i]
  end
  for i = 1, 1 do
    network.output_layer[i].weights[1] = network.output_layer[i].weights[1] - learning_rate * delta_output
    network.output_layer[i].weights[2] = network.output_layer[i].weights[2] - learning_rate * delta_output
  end
end

-- Train the network on a dataset.
local dataset = {
  {{0, 0}, 0},
  {{