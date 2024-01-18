```csharp
// This C# program simulates a simple neural network for classifying handwritten digits.

// Import necessary libraries.
using System;
using System.Collections.Generic;
using System.Linq;

// Define the neural network class.
public class NeuralNetwork
{
    // Define the number of input neurons, hidden neurons, and output neurons.
    private int inputNeurons;
    private int hiddenNeurons;
    private int outputNeurons;

    // Define the weights between the input and hidden layers, and the hidden and output layers.
    private double[,] inputHiddenWeights;
    private double[,] hiddenOutputWeights;

    // Define the biases for the hidden and output layers.
    private double[] hiddenBiases;
    private double[] outputBiases;

    // Define the learning rate.
    private double learningRate;

    // Initialize the neural network.
    public NeuralNetwork(int inputNeurons, int hiddenNeurons, int outputNeurons, double learningRate)
    {
        this.inputNeurons = inputNeurons;
        this.hiddenNeurons = hiddenNeurons;
        this.outputNeurons = outputNeurons;
        this.learningRate = learningRate;

        // Initialize the weights and biases randomly.
        inputHiddenWeights = new double[inputNeurons, hiddenNeurons];
        hiddenOutputWeights = new double[hiddenNeurons, outputNeurons];
        hiddenBiases = new double[hiddenNeurons];
        outputBiases = new double[outputNeurons];

        Random random = new Random();
        for (int i = 0; i < inputNeurons; i++)
        {
            for (int j = 0; j < hiddenNeurons; j++)
            {
                inputHiddenWeights[i, j] = random.NextDouble();
            }
        }

        for (int i = 0; i < hiddenNeurons; i++)
        {
            for (int j = 0; j < outputNeurons; j++)
            {
                hiddenOutputWeights[i, j] = random.NextDouble();
            }
        }

        for (int i = 0; i < hiddenNeurons; i++)
        {
            hiddenBiases[i] = random.NextDouble();
        }

        for (int i = 0; i < outputNeurons; i++)
        {
            outputBiases[i] = random.NextDouble();
        }
    }

    // Feed forward the neural network.
    public double[] FeedForward(double[] input)
    {
        // Calculate the hidden layer activations.
        double[] hiddenActivations = new double[hiddenNeurons];
        for (int i = 0; i < hiddenNeurons; i++)
        {
            double sum = 0.0;
            for (int j = 0; j < inputNeurons; j++)
            {
                sum += input[j] * inputHiddenWeights[j, i];
            }

            sum += hiddenBiases[i];

            hiddenActivations[i] = Sigmoid(sum);
        }

        // Calculate the output layer activations.
        double[] outputActivations = new double[outputNeurons];
        for (int i = 0; i < outputNeurons; i++)
        {
            double sum = 0.0;
            for (int j = 0; j < hiddenNeurons; j++)
            {
                sum += hiddenActivations[j] * hiddenOutputWeights[j, i];
            }

            sum += outputBiases[i];

            outputActivations[i] = Sigmoid(sum);
        }

        return outputActivations;
    }

    // Backpropagate the error.
    public void Backpropagate(double[] input, double[] expectedOutput, double[] actualOutput)
    {
        // Calculate the error at the output layer.
        double[] outputErrors = new double[outputNeurons];
        for (int i = 0; i < outputNeurons; i++)
        {
            outputErrors[i] = expectedOutput[i] - actualOutput[i];
        }

        // Calculate the error at the hidden layer.
        double[] hiddenErrors = new double[hiddenNeurons];
        for (int i = 0; i < hiddenNeurons; i++)
        {
            double sum = 0.0;
            for (int j = 0; j < outputNeurons; j++)
            {
                sum += outputErrors[j] * hiddenOutputWeights[i, j];
            }

            hiddenErrors[i] = sum;
        }

        // Update the weights and biases.
        for (int i = 0; i < inputNeurons; i++)
        {
            for (int j = 0; j < hiddenNeurons; j++)
            {
                inputHiddenWeights[i, j] += learningRate * input[i] * hiddenErrors[j];
            }
        }

        for (int i = 0; i < hiddenNeurons; i++)
        {
            for (int j = 0; j < outputNeurons; j++)
            {
                hiddenOutputWeights[i, j] += learningRate * hiddenActivations[i] * outputErrors[j];
            }
        }

        for (int i = 0; i < hiddenNeurons; i++)
        {
            hiddenBiases[i] += learningRate * hiddenErrors[i];
        }

        for (int i = 0; i < outputNeurons; i++)
        {
            outputBiases[i] += learningRate * outputErrors[i];
        }
    }

    // Train the neural network.
    public void Train(double[][] inputs, double[][] expectedOutputs, int epochs)
    {
        for (int i = 0; i < epochs; i++)
        {
            for (int j = 0; j < inputs.Length; j++)
            {
                // Feed forward the neural network.
                double[] actualOutput = FeedForward(inputs[j]);

                // Backpropagate the error.
                Backpropagate(inputs[j], expectedOutputs[j], actualOutput);
            }
        }
    }

    // Test the neural network.
    public double Test(double[][] inputs, double[][] expected