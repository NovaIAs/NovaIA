```lua
-- This Lua script implements a simple neural network for binary classification.

-- Import the required libraries.
local torch = require("torch")
local nn = require("nn")
local optim = require("optim")

-- Define the neural network architecture.
local model = nn.Sequential()
model:add(nn.Linear(784, 128))  -- Input layer to hidden layer
model:add(nn.ReLU())  -- Activation function for hidden layer
model:add(nn.Linear(128, 64))  -- Hidden layer to output layer
model:add(nn.ReLU())  -- Activation function for output layer
model:add(nn.Linear(64, 2))  -- Output layer

-- Define the loss function and optimizer.
local loss_fn = nn.CrossEntropyLoss()
local optimizer = optim.SGD(model:parameters(), {lr = 0.01})

-- Load the MNIST dataset.
local train_dataset = torch.data.MNIST("./data", "train")
local test_dataset = torch.data.MNIST("./data", "test")

-- Train the neural network.
for epoch = 1, 10 do
    -- Iterate over the training dataset.
    for i = 1, train_dataset:size() do
        -- Get the input and target data.
        local input = train_dataset:get(i).data
        local target = train_dataset:get(i).label

        -- Forward pass through the neural network.
        local output = model:forward(input)

        -- Calculate the loss.
        local loss = loss_fn(output, target)

        -- Backward pass through the neural network.
        model:zeroGradParameter()
        loss:backward()

        -- Update the weights of the neural network.
        optimizer:step()
    end

    -- Evaluate the neural network on the test dataset.
    local correct = 0
    local total = 0
    for i = 1, test_dataset:size() do
        -- Get the input and target data.
        local input = test_dataset:get(i).data
        local target = test_dataset:get(i).label

        -- Forward pass through the neural network.
        local output = model:forward(input)

        -- Check if the predicted label matches the target label.
        local predicted = torch.max(output, 2)[1]
        if predicted == target then
            correct = correct + 1
        end

        -- Update the total number of samples.
        total = total + 1
    end

    -- Print the accuracy of the neural network on the test dataset.
    print("Epoch", epoch, "Accuracy:", correct / total)
end

-- Save the trained neural network.
torch.save(model, "mnist_classifier.pt")
```

This code implements a simple neural network for binary classification. The neural network has two hidden layers with 128 and 64 units, respectively, and an output layer with 2 units. The neural network is trained on the MNIST dataset, which consists of 70,000 grayscale images of handwritten digits. The neural network is trained for 10 epochs, and the accuracy of the neural network on the test dataset is printed after each epoch. The trained neural network is then saved to a file.