// This code defines a JavaScript class called "NeuralNetwork"
// that represents a basic artificial neural network.

// The constructor function for the NeuralNetwork class takes two
// parameters: "layers" and "learningRate".

// The "layers" parameter is an array of objects, where each object
// represents a layer in the neural network. Each layer object has
// two properties: "size" and "activation". The "size" property
// specifies the number of neurons in the layer, and the "activation"
// property specifies the activation function to be used by the neurons
// in the layer.

// The "learningRate" parameter specifies the learning rate for the neural
// network, which determines how quickly the network learns from its
// mistakes.

function NeuralNetwork(layers, learningRate) {
  // Initialize the neural network's layers and learning rate.
  this.layers = layers;
  this.learningRate = learningRate;

  // Initialize the weights and biases for each layer in the neural
  // network. The weights are initialized to random values, and the
  // biases are initialized to zero.
  for (let i = 1; i < this.layers.length; i++) {
    const layer = this.layers[i];
    const previousLayerSize = this.layers[i - 1].size;
    layer.weights = [];
    layer.biases = [];
    for (let j = 0; j < layer.size; j++) {
      layer.weights.push(Math.random() * 2 - 1);
      layer.biases.push(0);
    }
  }

  // Define the activation functions to be used by the neurons in the
  // neural network.
  this.activationFunctions = {
    sigmoid: function (x) {
      return 1 / (1 + Math.exp(-x));
    },
    relu: function (x) {
      return Math.max(0, x);
    },
    tanh: function (x) {
      return Math.tanh(x);
    },
  };
}

// The "feedForward" method of the NeuralNetwork class takes an input
// vector "input" and propagates it through the neural network,
// returning the output vector.

NeuralNetwork.prototype.feedForward = function (input) {
  // Initialize the output vector.
  let output = input;

  // Propagate the input vector through each layer of the neural
  // network, applying the activation function to the weighted sum
  // of the inputs to each neuron.
  for (let i = 1; i < this.layers.length; i++) {
    const layer = this.layers[i];
    const weightedSum = [];
    for (let j = 0; j < layer.size; j++) {
      weightedSum.push(0);
      for (let k = 0; k < input.length; k++) {
        weightedSum[j] += input[k] * layer.weights[j][k];
      }
      weightedSum[j] += layer.biases[j];
    }
    output = weightedSum.map(this.activationFunctions[layer.activation]);
  }

  // Return the output vector.
  return output;
};

// The "train" method of the NeuralNetwork class takes a training
// dataset "data" and trains the neural network to learn the mapping
// from inputs to outputs.

NeuralNetwork.prototype.train = function (data) {
  // Loop through the training data.
  for (let i = 0; i < data.length; i++) {
    // Get the input and output vectors from the training data.
    const input = data[i].input;
    const output = data[i].output;

    // Feed the input vector through the neural network to get the
    // predicted output vector.
    const predictedOutput = this.feedForward(input);

    // Calculate the error between the predicted output and the desired
    // output.
    const error = output.map((o, i) => o - predictedOutput[i]);

    // Calculate the gradients of the weights and biases for each layer
    // in the neural network.
    const gradients = [];
    for (let j = this.layers.length - 1; j > 0; j--) {
      const layer = this.layers[j];
      gradients.push([]);
      for (let k = 0; k < layer.size; k++) {
        gradients[j].push([]);
        for (let l = 0; l < input.length; l++) {
          gradients[j][k].push(0);
        }
        gradients[j][k].push(0);
      }
    }
    for (let j = this.layers.length - 1; j > 0; j--) {
      const layer = this.layers[j];
      const previousLayerOutput = j === 1 ? input : this.layers[j - 1].output;
      for (let k = 0; k < layer.size; k++) {
        for (let l = 0; l < input.length; l++) {
          gradients[j][k][l] =
            error[k] *
            this.activationFunctions[layer.activation](
              weightedSum[k]
            ) *
            previousLayerOutput[l];
        }
        gradients[j][k][input.length] =
          error[k] * this.activationFunctions[layer.activation](weightedSum[k]);
      }
    }

    // Update the weights and biases for each layer in the neural
    // network using the gradients.
    for (let j = 1; j < this.layers.length; j++) {
      const layer = this.layers[j];
      for (let k = 0; k < layer.size; k++) {
        for (let l = 0; l < input.length; l++) {
          layer.weights[k][l] -= this.learningRate * gradients[j][k][l];
        }
        layer.biases[k] -= this.learningRate * gradients[j][k][input.length];
      }
    }
  }
};

// Create a neural network with two hidden layers, each with 10 neurons,
// and an output layer with 1 neuron.
const neuralNetwork = new NeuralNetwork(
  [
    { size: 2, activation: "relu" },
    { size: 10, activation: "relu" },
    { size: 10, activation: "relu" },
    { size: 1, activation: "sigmoid" },
  ],
  0.1
);

// Train the neural network on a dataset of XOR inputs and outputs.
const data = [
  { input: [0, 0], output: [0] },
  { input: [0, 1], output: [1] },
  { input: [1, 0], output: [1] },
  { input: [1, 1], output: [0] },
];
neuralNetwork.train(data);

// Use the neural network to make predictions on new inputs.
const input1 = [0, 0];
const output1 = neuralNetwork.feedForward(input1);
console.log(`Input: ${input1}, Output: ${output1}`);

const input2 = [0, 1];
const output2 = neuralNetwork.feedForward(input2);
console.log(`Input: ${input2}, Output: ${output2}`);

const input3 = [1, 0];
const output3 = neuralNetwork.feedForward(input3);
console.log(`Input: ${input3}, Output: ${output3}`);

const input4 = [1, 1];
const output4 = neuralNetwork.feedForward(input4);
console.log(`Input: ${input4}, Output: ${output4}`);