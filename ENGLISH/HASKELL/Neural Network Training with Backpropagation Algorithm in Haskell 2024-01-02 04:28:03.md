Sure! Here's a complex Haskell code that implements a neural network with backpropagation algorithm for training. It's a large and differentiated code that provides a detailed explanation of each component.

```haskell
import Numeric.LinearAlgebra

-- Activation function
sigmoid :: Floating a => a -> a
sigmoid x = 1 / (1 + exp (-x))

-- Derivative of the activation function
sigmoid' :: Floating a => a -> a
sigmoid' x = sigmoid x * (1 - sigmoid x)

-- Neural network data type
data NeuralNetwork = NeuralNetwork
    { weights :: [Matrix Double] -- List of weight matrices
    , biases :: [Vector Double]  -- List of bias vectors
    }

-- Feed forward computation
feedForward :: NeuralNetwork -> Vector Double -> Vector Double
feedForward nn input = foldl forward input (zip (weights nn) (biases nn))
  where
    forward a (w, b) = sigmoid (w #> a + b)

-- Backpropagation algorithm for training the neural network
backward :: NeuralNetwork -> Vector Double -> Vector Double -> (NeuralNetwork, Vector Double)
backward nn input target = (nn', gradient)
  where
    activations = scanl forward input (zip (weights nn) (biases nn))
    forward a (w, b) = sigmoid (w #> a + b)

    delta = last activations - target
    gradient = foldl backpropagate delta (tail $ zip (reverse (weights nn)) (tail activations))

    backpropagate delta (w, a) = (w, delta)
      where
        z = w #> a
        delta' = (transpose w #> delta) * cmap sigmoid' z

-- Update weights and biases using gradient descent
updateWeightsBiases :: NeuralNetwork -> Double -> Vector Double -> NeuralNetwork
updateWeightsBiases nn learningRate gradient = NeuralNetwork
    { weights = zipWith (\w gw -> w - scalar learningRate * gw) (weights nn) (map fst gradient)
    , biases = zipWith (\b gb -> b - scalar learningRate * gb) (biases nn) (map snd gradient)
    }

-- Example usage
main :: IO ()
main = do
    let nn = NeuralNetwork
            { weights = [ (2><2) [1, 2, 3, 4] -- Weight matrix for the first layer
                        , (1><2) [5, 6]        -- Weight matrix for the output layer
                        ]
            , biases = [ fromList [1, 2]      -- Bias vector for the first layer
                       , fromList [3]         -- Bias vector for the output layer
                       ]
            }
        input = fromList [0.5, 0.8]           -- Input vector
        target = fromList [0.2]               -- Target vector

        learningRate = 0.1                    -- Learning rate
        iterations = 100                      -- Number of training iterations

    let trainedNN = train nn input target learningRate iterations
    putStrLn $ "Trained neural network: " ++ show trainedNN
  where
    train nn input target learningRate 0 = nn
    train nn input target learningRate n =
        let (nn', gradient) = backward nn input target
            nn'' = updateWeightsBiases nn' learningRate gradient
        in train nn'' input target learningRate (n - 1)
```

This code implements a simple neural network in Haskell. The `NeuralNetwork` data type represents the network, with weight matrices and bias vectors. The `feedForward` function performs the feedforward computation of the network. The `backward` function implements the backpropagation algorithm to compute the gradients. The `updateWeightsBiases` function updates the weights and biases of the network using gradient descent.

In the `main` function, an example usage is shown. The neural network is initialized with some random weights and biases. It is then trained using the `train` function, which performs the backward pass and updates the network parameters iteratively.

Note that this is a simplified implementation and may not be suitable for more complex neural networks. However, it provides a good starting point for understanding the basic concepts of neural networks and backpropagation.