Certainly! Below is a complex and unique PHP code that implements a multi-layer perceptron (MLP) neural network with backpropagation for classification tasks. This code can be used to train and evaluate a neural network model on a given dataset.

```php
<?php
class MLP
{
    private $inputLayerSize;
    private $hiddenLayerSize;
    private $outputLayerSize;
    private $weightsIH;
    private $weightsHO;

    public function __construct($inputLayerSize, $hiddenLayerSize, $outputLayerSize)
    {
        $this->inputLayerSize = $inputLayerSize;
        $this->hiddenLayerSize = $hiddenLayerSize;
        $this->outputLayerSize = $outputLayerSize;

        // Initialize the weights between the input and hidden layers
        $this->weightsIH = $this->initializeWeights($this->inputLayerSize, $this->hiddenLayerSize);

        // Initialize the weights between the hidden and output layers
        $this->weightsHO = $this->initializeWeights($this->hiddenLayerSize, $this->outputLayerSize);
    }

    private function initializeWeights($rows, $cols)
    {
        $weights = [];
        for ($i = 0; $i < $rows; $i++) {
            $weights[$i] = [];
            for ($j = 0; $j < $cols; $j++) {
                $weights[$i][$j] = $this->randomWeight();
            }
        }
        return $weights;
    }

    private function randomWeight()
    {
        return mt_rand(-1000, 1000) / 1000; // Random weight between -1 and 1
    }

    private function sigmoid($x)
    {
        return 1 / (1 + exp(-$x));
    }

    private function sigmoidDerivative($x)
    {
        $sigmoid = $this->sigmoid($x);
        return $sigmoid * (1 - $sigmoid);
    }

    public function train($X, $y, $epochs, $learningRate)
    {
        for ($epoch = 0; $epoch < $epochs; $epoch++) {
            for ($i = 0; $i < count($X); $i++) {
                // Forward propagation
                $hiddenLayerInput = $this->dotProduct($X[$i], $this->weightsIH);
                $hiddenLayerOutput = $this->applyActivation($hiddenLayerInput);

                $outputLayerInput = $this->dotProduct($hiddenLayerOutput, $this->weightsHO);
                $outputLayerOutput = $this->applyActivation($outputLayerInput);

                // Backpropagation
                $outputLayerError = $y[$i] - $outputLayerOutput;
                $outputLayerGradient = $outputLayerError * $this->sigmoidDerivative($outputLayerInput);
                $hiddenLayerError = $this->dotProduct($this->weightsHO, $outputLayerGradient);
                $hiddenLayerGradient = $hiddenLayerError * $this->sigmoidDerivative($hiddenLayerInput);

                // Update weights
                for ($j = 0; $j < $this->hiddenLayerSize; $j++) {
                    for ($k = 0; $k < $this->outputLayerSize; $k++) {
                        $this->weightsHO[$j][$k] += $learningRate * $outputLayerGradient[$k] * $hiddenLayerOutput[$j];
                    }
                }

                for ($j = 0; $j < $this->inputLayerSize; $j++) {
                    for ($k = 0; $k < $this->hiddenLayerSize; $k++) {
                        $this->weightsIH[$j][$k] += $learningRate * $hiddenLayerGradient[$k] * $X[$i][$j];
                    }
                }
            }
        }
    }

    public function predict($X)
    {
        $predictions = [];
        foreach ($X as $sample) {
            $hiddenLayerOutput = $this->applyActivation($this->dotProduct($sample, $this->weightsIH));
            $outputLayerOutput = $this->applyActivation($this->dotProduct($hiddenLayerOutput, $this->weightsHO));
            $predictions[] = $outputLayerOutput;
        }
        return $predictions;
    }

    private function applyActivation($x)
    {
        if (is_array($x)) {
            return array_map([$this, 'sigmoid'], $x);
        } else {
            return $this->sigmoid($x);
        }
    }

    private function dotProduct($a, $b)
    {
        $result = [];
        foreach ($a as $i => $row) {
            $result[$i] = 0;
            foreach ($row as $j => $value) {
                $result[$i] += $value * $b[$j];
            }
        }
        return $result;
    }
}

// Usage example
$inputLayerSize = 2;
$hiddenLayerSize = 4;
$outputLayerSize = 1;

$X = [[0, 0], [0, 1], [1, 0], [1, 1]];
$y = [[0], [1], [1], [0]];

$mlp = new MLP($inputLayerSize, $hiddenLayerSize, $outputLayerSize);
$mlp->train($X, $y, 1000, 0.1);

$testData = [[0, 0], [0, 1], [1, 0], [1, 1]];
$predictions = $mlp->predict($testData);

echo "Input\t\t\tPrediction\n";
foreach ($testData as $i => $inputs) {
    echo implode("\t\t", $inputs) . "\t\t" . round($predictions[$i]) . "\n";
}
?>
```

This code defines a class called `MLP` which represents a multi-layer perceptron neural network. The constructor initializes the size of the input, hidden, and output layers, as well as the weights between them. The `initializeWeights` method randomly initializes the weights.

The `train` method performs the training of the neural network using the backpropagation algorithm. It takes the input data (`$X`) and the corresponding labels (`$y`), the number of epochs, and the learning rate as parameters. During each epoch, the forward propagation and backpropagation steps are performed to update the weights.

The `predict` method takes a set of input data (`$X`) and returns the predictions made by the trained neural network.

The remaining methods (`sigmoid`, `sigmoidDerivative`, `applyActivation`, and `dotProduct`) are helper functions used in the forward and backpropagation steps.

In the usage example, a 2-input XOR problem is used to demonstrate the neural network. The input data (`$X`) represents the XOR inputs, and the labels (`$y`) represent the corresponding XOR outputs. The neural network is trained for 1000 epochs with a learning rate of 0.1. Then, the trained model is used to predict the output for the same input data, and the results are printed.

Please note that this code is a simplified implementation for educational purposes and may not be suitable for large-scale applications.