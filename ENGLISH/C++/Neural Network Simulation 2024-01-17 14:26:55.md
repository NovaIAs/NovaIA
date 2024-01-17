```c++
// This program simulates the behavior of a neural network.

// Import necessary libraries.
#include <iostream>
#include <vector>
#include <random>

// Define the structure of a neuron.
struct Neuron {
    std::vector<double> weights;  // The weights of the neuron's connections.
    double bias;                  // The bias of the neuron.
    double output;                // The output of the neuron.
};

// Define the structure of a neural network.
struct NeuralNetwork {
    std::vector<Neuron> neurons;  // The neurons of the neural network.
    std::vector<std::vector<double>> inputs;  // The inputs to the neural network.
    std::vector<double> outputs;             // The outputs of the neural network.
};

// Initialize a neural network.
NeuralNetwork initialize_network(int num_inputs, int num_neurons, int num_outputs) {
    NeuralNetwork network;

    // Create the neurons.
    for (int i = 0; i < num_neurons; i++) {
        Neuron neuron;

        // Initialize the weights of the neuron.
        for (int j = 0; j < num_inputs; j++) {
            neuron.weights.push_back(random_weight());
        }

        // Initialize the bias of the neuron.
        neuron.bias = random_bias();

        // Add the neuron to the network.
        network.neurons.push_back(neuron);
    }

    // Create the connections between the neurons.
    for (int i = 0; i < num_neurons; i++) {
        for (int j = 0; j < num_outputs; j++) {
            network.neurons[i].connections.push_back(j);
        }
    }

    return network;
}

// Train a neural network.
void train_network(NeuralNetwork &network, std::vector<std::vector<double>> inputs, std::vector<double> outputs) {
    // Iterate over the training data.
    for (int i = 0; i < inputs.size(); i++) {
        // Set the inputs to the neural network.
        network.inputs = inputs[i];

        // Calculate the outputs of the neural network.
        network.outputs = calculate_outputs(network);

        // Calculate the error between the outputs of the neural network and the desired outputs.
        std::vector<double> errors;
        for (int j = 0; j < outputs.size(); j++) {
            errors.push_back(outputs[j] - network.outputs[j]);
        }

        // Update the weights and biases of the neural network.
        for (int j = 0; j < network.neurons.size(); j++) {
            for (int k = 0; k < network.neurons[j].connections.size(); k++) {
                network.neurons[j].weights[k] += learning_rate * errors[k] * network.inputs[j];
            }
            network.neurons[j].bias += learning_rate * errors[k];
        }
    }
}

// Calculate the outputs of a neural network.
std::vector<double> calculate_outputs(NeuralNetwork &network) {
    std::vector<double> outputs;

    // Iterate over the neurons in the neural network.
    for (int i = 0; i < network.neurons.size(); i++) {
        // Calculate the output of the neuron.
        double output = 0.0;
        for (int j = 0; j < network.neurons[i].inputs.size(); j++) {
            output += network.neurons[i].weights[j] * network.neurons[i].inputs[j];
        }
        output += network.neurons[i].bias;

        // Apply the activation function to the output of the neuron.
        output = activation_function(output);

        // Add the output of the neuron to the list of outputs.
        outputs.push_back(output);
    }

    return outputs;
}

// Generate a random weight.
double random_weight() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dist(-1.0, 1.0);
    return dist(gen);
}

// Generate a random bias.
double random_bias() {
    std::random_device rd;
    std::mt19937 gen(rd());
    std::uniform_real_distribution<double> dist(-1.0, 1.0);
    return dist(gen);
}

// Apply the activation function to a value.
double activation_function(double value) {
    return 1.0 / (1.0 + exp(-value));
}

// Main function.
int main() {
    // Create a neural network.
    NeuralNetwork network = initialize_network(2, 3, 1);

    // Train the neural network.
    std::vector<std::vector<double>> inputs = {{0.0, 0.0}, {0.0, 1.0}, {1.0, 0.0}, {1.0, 1.0}};
    std::vector<double> outputs = {0.0, 1.0, 1.0, 0.0};
    train_network(network, inputs, outputs);

    // Test the neural network.
    std::vector<std::vector<double>> test_inputs = {{0.5, 0.5}};
    std::vector<double> test_outputs = calculate_outputs(network, test_inputs);

    // Print the results.
    for (int i = 0; i < test_outputs.size(); i++) {
        std::cout << test_outputs[i] << std::endl;
    }

    return 0;
}
```

This code simulates the behavior of a neural network. A neural network is a type of machine learning model that is inspired by the human brain. Neural networks are made up of layers of interconnected neurons, which are simple processing units. Each neuron takes in a set of inputs, calculates a weighted sum of those inputs, and then passes the result through an activation function to produce an output.

The code begins by defining the structure of a neuron and a neural network. A neuron has a set of weights, a bias, and an output. A neural network has a set of neurons, a set of inputs, and a set of outputs.

The code then initializes a neural network with 2 inputs, 3 neurons, and 1 output. The network is trained on a set of 4 input-output pairs. The training algorithm updates the weights and biases of the network in order to minimize the error between the network's outputs and the desired outputs.

Once the network is trained, it is tested on a new set of inputs. The network's outputs are printed to the console.

This code is a complex example of a neural network simulation. It is a good example of how neural networks can be used to solve machine learning problems.