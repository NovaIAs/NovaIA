```lua
-- This Lua script implements a basic artificial neural network (ANN) for binary classification.
-- It consists of an input layer, a hidden layer, and an output layer.

-- Import the required libraries.
require "torch"
require "nn"

-- Define the ANN architecture.
local model = nn.Sequential()
model:add(nn.Linear(2, 4))  -- Input layer to hidden layer (2 input neurons, 4 hidden neurons)
model:add(nn.ReLU())  -- Activation function for hidden layer
model:add(nn.Linear(4, 1))  -- Hidden layer to output layer (4 hidden neurons, 1 output neuron)
model:add(nn.Sigmoid())  -- Activation function for output layer

-- Define the loss function and optimizer.
local criterion = nn.BCECriterion()  -- Binary cross-entropy loss function
local optimizer = optim.SGD(model.parameters(), 0.1)  -- Stochastic gradient descent optimizer with a learning rate of 0.1

-- Define the training data and labels.
local X = torch.FloatTensor({{0, 0}, {0, 1}, {1, 0}, {1, 1}})  -- Input data (4 samples, 2 features)
local y = torch.FloatTensor({{0}, {1}, {1}, {0}})  -- Output labels (4 samples, 1 label)

-- Train the ANN.
for epoch = 1, 1000 do
    -- Forward pass: compute the output of the ANN.
    local output = model:forward(X)

    -- Compute the loss.
    local loss = criterion:forward(output, y)

    -- Backward pass: compute the gradients of the loss with respect to the weights and biases.
    model:zeroGradParameter()  -- Reset the gradients to zero before backward pass
    loss:backward()

    -- Update the weights and biases using the optimizer.
    optimizer:step()

    -- Print the loss every 100 epochs.
    if epoch % 100 == 0 then
        print("Epoch:", epoch, "Loss:", loss)
    end
end

-- Evaluate the ANN.
local accuracy = 0
for i = 1, X:size(1) do
    local output = model:forward(X[i])
    local prediction = output > 0.5 and 1 or 0  -- Threshold the output to obtain a binary prediction
    accuracy = accuracy + (prediction == y[i])
end
accuracy = accuracy / X:size(1)

-- Print the accuracy.
print("Accuracy:", accuracy)
```

**Explanation:**

* The `require` statements import the necessary libraries, `torch` and `nn`, for building and training the ANN.
* The `nn.Sequential()` constructor is used to define the architecture of the ANN, which consists of an input layer, a hidden layer, and an output layer. The `nn.Linear()` module is used to define the linear transformations between layers, and the `nn.ReLU()` and `nn.Sigmoid()` modules are used as activation functions.
* The `criterion` variable is set to the binary cross-entropy loss function, and the `optimizer` variable is set to the stochastic gradient descent optimizer.
* The training data and labels are defined as torch tensors.
* The ANN is trained using a loop that iterates through the training data for a specified number of epochs. In each epoch, the forward pass, loss computation, backward pass, and weight update steps are performed.
* The loss is printed every 100 epochs to monitor the training progress.
* After training, the ANN is evaluated on the training data to compute the accuracy.
* The final accuracy is printed.