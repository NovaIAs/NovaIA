```lua
-- Require the necessary libraries
local torch = require("torch")
local nn = require("nn")
local optim = require("optim")

-- Load the MNIST dataset
local mnist = require("mnist")
local train_data, train_labels = mnist.train_data:float()/255, mnist.train_labels
local test_data, test_labels = mnist.test_data:float()/255, mnist.test_labels

-- Define the neural network model
local model = nn.Sequential()
model:add(nn.Linear(784, 128))
model:add(nn.ReLU())
model:add(nn.Linear(128, 64))
model:add(nn.ReLU())
model:add(nn.Linear(64, 10))

-- Define the loss function and optimizer
local criterion = nn.CrossEntropyLoss()
local optimizer = optim.SGD(model.parameters(), 0.1)

-- Train the model
for epoch in 1, 10 do
    for batch in 1, #train_data/128 do
        local batch_data = train_data:narrow(1, (batch-1)*128+1, 128)
        local batch_labels = train_labels:narrow(1, (batch-1)*128+1, 128)

        optimizer:zeroGrad()
        local output = model:forward(batch_data)
        local loss = criterion:forward(output, batch_labels)
        loss:backward()
        optimizer:step()
    end
    print("Epoch:", epoch, "Loss:", loss)
end

-- Evaluate the model
local correct = 0
for i = 1, #test_data do
    local output = model:forward(test_data[i])
    if output:max(2)[2] == test_labels[i]+1 then
        correct = correct + 1
    end
end
print("Accuracy:", correct/#test_data)
```

Explanation:

1. We first require the necessary libraries.
2. We then load the MNIST dataset, which is a dataset of handwritten digits.
3. We define the neural network model, which consists of three fully connected layers.
4. We define the loss function and optimizer.
5. We train the model for 10 epochs, and for each epoch we iterate over the training data in batches.
6. For each batch, we calculate the output of the model, the loss, and then update the model's parameters using the optimizer.
7. After each epoch, we print the loss.
8. Finally, we evaluate the model on the test data and print the accuracy.