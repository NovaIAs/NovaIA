```lua
-- Este código simula una red neuronal simple para clasificar imágenes de dígitos escritos a mano.

-- Carga las bibliotecas necesarias.
local torch = require "torch"
local nn = require "nn"
local optim = require "optim"

-- Define la arquitectura de la red neuronal.
local net = nn.Sequential()
net:add(nn.Linear(784, 128))
net:add(nn.ReLU())
net:add(nn.Linear(128, 10))

-- Define la función de pérdida y el optimizador.
local criterion = nn.CrossEntropyLoss()
local optimizer = optim.SGD(net.parameters(), lr=0.01)

-- Carga el conjunto de datos.
local train_dataset = torch.data.mnist("/tmp/mnist", true)
local test_dataset = torch.data.mnist("/tmp/mnist", false)

-- Entrena la red neuronal.
for epoch = 1, 10 do
    for batch in train_dataset:batch(128) do
        optimizer:zeroGrad()
        local output = net:forward(batch.data)
        local loss = criterion:forward(output, batch.target)
        loss:backward()
        optimizer:step()
    end
end

-- Evalúa la red neuronal.
local correct = 0
local total = 0
for batch in test_dataset:batch(128) do
    local output = net:forward(batch.data)
    local predicted = output:max(2)[2]
    correct = correct + (predicted == batch.target).sum()
    total = total + batch.data:size(1)
end

local accuracy = correct / total

print("Accuracy:", accuracy)