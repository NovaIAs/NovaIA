```python
class NeuralNetwork:
    def __init__(self, layers, activation, loss, optimizer):
        self.layers = layers
        self.activation = activation
        self.loss = loss
        self.optimizer = optimizer

    def forward(self, X):
        for layer in self.layers:
            X = layer.forward(X)
        return X

    def backward(self, Y_pred, Y_true):
        dout = self.loss.backward(Y_pred, Y_true)
        for layer in reversed(self.layers):
            dout = layer.backward(dout)

    def update(self):
        for layer in self.layers:
            layer.update(self.optimizer)

    def train(self, X, Y, epochs, batch_size):
        for epoch in range(epochs):
            for i in range(0, len(X), batch_size):
                X_batch = X[i:i+batch_size]
                Y_batch = Y[i:i+batch_size]

                Y_pred = self.forward(X_batch)
                self.backward(Y_pred, Y_batch)
                self.update()

    def predict(self, X):
        Y_pred = self.forward(X)
        return Y_pred

class Layer:
    def __init__(self, input_dim, output_dim, activation):
        self.input_dim = input_dim
        self.output_dim = output_dim
        self.activation = activation

    def forward(self, X):
        raise NotImplementedError

    def backward(self, dout):
        raise NotImplementedError

class LinearLayer(Layer):
    def __init__(self, input_dim, output_dim, activation):
        super().__init__(input_dim, output_dim, activation)
        self.W = np.random.randn(input_dim, output_dim)
        self.b = np.zeros((1, output_dim))

    def forward(self, X):
        Z = np.dot(X, self.W) + self.b
        A = self.activation.forward(Z)
        return A

    def backward(self, dout):
        dZ = self.activation.backward(dout)
        self.dW = np.dot(X.T, dZ)
        self.db = np.sum(dZ, axis=0, keepdims=True)
        return np.dot(dZ, self.W.T)

class Activation:
    def __init__(self, name):
        self.name = name

    def forward(self, X):
        raise NotImplementedError

    def backward(self, dout):
        raise NotImplementedError

class Sigmoid(Activation):
    def __init__(self):
        super().__init__("Sigmoid")

    def forward(self, X):
        return 1 / (1 + np.exp(-X))

    def backward(self, dout):
        return dout * (1 - dout)

class Tanh(Activation):
    def __init__(self):
        super().__init__("Tanh")

    def forward(self, X):
        return np.tanh(X)

    def backward(self, dout):
        return (1 - np.tanh(X)**2) * dout

class ReLU(Activation):
    def __init__(self):
        super().__init__("ReLU")

    def forward(self, X):
        return np.maximum(0, X)

    def backward(self, dout):
        return np.where(X > 0, dout, 0)

class Loss:
    def __init__(self, name):
        self.name = name

    def forward(self, Y_pred, Y_true):
        raise NotImplementedError

    def backward(self, Y_pred, Y_true):
        raise NotImplementedError

class MSE(Loss):
    def __init__(self):
        super().__init__("MSE")

    def forward(self, Y_pred, Y_true):
        return np.mean((Y_pred - Y_true)**2)

    def backward(self, Y_pred, Y_true):
        return 2 * (Y_pred - Y_true) / Y_pred.shape[0]

class Optimizer:
    def __init__(self, learning_rate):
        self.learning_rate = learning_rate

    def update(self, layer):
        raise NotImplementedError

class SGD(Optimizer):
    def __init__(self, learning_rate):
        super().__init__(learning_rate)

    def update(self, layer):
        layer.W -= self.learning_rate * layer.dW
        layer.b -= self.learning_rate * layer.db

class Adam(Optimizer):
    def __init__(self, learning_rate, beta1=0.9, beta