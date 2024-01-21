```lua
-- Este código simula una red neuronal artificial sencilla con retropropagación.
-- La red tiene una capa de entrada, una capa oculta y una capa de salida.
-- La capa de entrada tiene 2 neuronas, la capa oculta tiene 3 neuronas y la capa de salida tiene 1 neurona.
-- La red se entrena con un conjunto de datos de 4 ejemplos, cada uno con 2 características de entrada y 1 etiqueta de salida.

-- Definir las funciones de activación
function sigmoid(x)
  return 1 / (1 + math.exp(-x))
end

function dsigmoid(x)
  return x * (1 - x)
end

-- Definir la clase de la red neuronal
class NeuralNetwork() {
  function __init__(self)
    -- Crear las capas de la red
    self.layers = {}
    self.layers[1] = Layer(2, 3) -- Capa de entrada
    self.layers[2] = Layer(3, 1) -- Capa oculta
    self.layers[3] = Layer(1, 1) -- Capa de salida

    -- Inicializar los pesos y sesgos de la red
    for i = 1, #self.layers do
      for j = 1, #self.layers[i].neurons do
        self.layers[i].neurons[j].weights = torch.rand(1, #self.layers[i - 1].neurons)
        self.layers[i].neurons[j].bias = torch.rand(1)
      end
    end
  end

  function forward(self, x)
    -- Realizar la propagación hacia adelante
    for i = 1, #self.layers do
      if i == 1 then
        self.layers[i].activations = x
      else
        self.layers[i].activations = sigmoid(self.layers[i - 1].activations * self.layers[i].neurons[1].weights + self.layers[i].neurons[1].bias)
      end
    end

    -- Devolver la salida de la red
    return self.layers[3].activations
  end

  function backward(self, y)
    -- Calcular el error de la red
    self.layers[3].error = self.layers[3].activations - y

    -- Calcular los gradientes de la red
    for i = #self.layers, 1, -1 do
      if i == #self.layers then
        self.layers[i].gradient = self.layers[i].error * dsigmoid(self.layers[i].activations)
      else
        self.layers[i].gradient = (self.layers[i + 1].neurons[1].weights' * self.layers[i + 1].gradient) * dsigmoid(self.layers[i].activations)
      end
    end

    -- Actualizar los pesos y sesgos de la red
    for i = 1, #self.layers do
      for j = 1, #self.layers[i].neurons do
        self.layers[i].neurons[j].weights = self.layers[i].neurons[j].weights - self.layers[i].gradient * self.layers[i - 1].activations
        self.layers[i].neurons[j].bias = self.layers[i].neurons[j].bias - self.layers[i].gradient
      end
    end
  end

  function train(self, x, y)
    -- Realizar la propagación hacia adelante
    self.forward(x)

    -- Calcular el error de la red
    self.backward(y)
  end

  function predict(self, x)
    -- Realizar la propagación hacia adelante
    return self.forward(x)
  end
}

-- Definir la clase de la capa neuronal
class Layer() {
  function __init__(self, num_inputs, num_neurons)
    -- Crear las neuronas de la capa
    self.neurons = {}
    for i = 1, num_neurons do
      self.neurons[i] = Neuron(num_inputs)
    end
  end

  function forward(self, x)
    -- Realizar la propagación hacia adelante
    for i = 1, #self.neurons do
      self.neurons[i].activation = sigmoid(self.neurons[i].weights * x + self.neurons[i].bias)
    end

    -- Devolver las activaciones de la capa
    return torch.cat({self.neurons[1].activation}, 1)
  end

  function backward(self, gradient)
    -- Calcular los gradientes de la capa
    for i = 1, #self.neurons do
      self.neurons[i].gradient = gradient * dsigmoid(self.neurons[i].activation)
    end

    -- Devolver los gradientes de la capa
    return torch.cat({self.neurons[1].gradient}, 1)
  end
}

-- Definir la clase de la neurona
class Neuron() {
  function __init__(self, num_inputs)
    -- Crear los pesos y sesgos de la neurona
    self.weights = torch.rand(1, num_inputs)
    self.bias = torch.rand(1)

    -- Inicializar la activación de la neurona
    self.activation = 0
  end
}

-- Crear la red neuronal
network = NeuralNetwork()

-- Entrenar la red neuronal
for i = 1, 1000 do
  -- Obtener un ejemplo del conjunto de datos
  x = torch.rand(1, 2)
  y = torch.tensor(x[1] + x[2])

  -- Entrenar la red neuronal con el ejemplo
  network.train(x, y)
end

-- Probar la red neuronal
x = torch.rand(1, 2)
y = torch.tensor(x[1] + x[2])

print("Entrada: ", x)
print("Salida esperada: ", y)
print("Salida real: ", network.predict(x))
```

**Explicación del código:**

* El código define una clase `NeuralNetwork` que implementa una red neuronal artificial sencilla con retropropagación.
* La clase `NeuralNetwork` tiene una función `__init__` que inicializa los pesos y sesgos de la red.
* La clase `NeuralNetwork` tiene una función `forward` que realiza la propagación hacia adelante a través de la red.
* La clase `NeuralNetwork` tiene una función `backward` que realiza la propagación hacia atrás a través de la red y calcula los gradientes de los pesos y sesgos.
* La clase `NeuralNetwork` tiene una función `train` que entrena la red con un conjunto de datos.
* La clase `NeuralNetwork` tiene una función `predict` que predice la salida de la red para una entrada dada.
* El código también define una clase `Layer` que implementa una capa neuronal.
* La clase `Layer` tiene una función `__init__` que inicializa las neuronas de la capa.
* La clase `Layer` tiene una función `forward` que realiza la propagación hacia adelante a través de la capa.
* La clase `Layer` tiene una función `backward` que realiza la propagación hacia atrás a través de la capa y calcula los gradientes de los pesos y sesgos.
* El código también define una clase `Neuron` que implementa una neurona.
* La clase `Neuron` tiene una función `__init__` que inicializa los pesos y sesgos de la neurona.
* El código crea una red neuronal con una capa de entrada de 2 neuronas, una capa oculta de 3 neuronas y una capa de salida de 1 neurona.
* El código entrena la red neuronal con un conjunto de datos de 4 ejemplos.
* El código prueba la red neuronal con un ejemplo nuevo.