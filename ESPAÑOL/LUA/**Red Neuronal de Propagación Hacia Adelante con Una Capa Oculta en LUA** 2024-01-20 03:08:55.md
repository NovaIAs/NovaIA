```lua
-- Este es un programa en LUA que implementa una red neuronal básica de propagación hacia adelante.

-- Definimos las funciones de activación que se usarán en la red neuronal.

function sigmoid(x)
    return 1 / (1 + math.exp(-x))
end

function relu(x)
    return math.max(0, x)
end

-- Definimos la arquitectura de la red neuronal.

n_input = 3 -- Número de neuronas en la capa de entrada.
n_hidden = 5 -- Número de neuronas en la capa oculta.
n_output = 2 -- Número de neuronas en la capa de salida.

-- Inicializamos los pesos y sesgos de la red neuronal.

weights_input_hidden = torch.randn(n_input, n_hidden)
biases_hidden = torch.zeros(1, n_hidden)

weights_hidden_output = torch.randn(n_hidden, n_output)
biases_output = torch.zeros(1, n_output)

-- Definimos la función de propagación hacia adelante de la red neuronal.

function forward_pass(x)
    -- Calculamos la salida de la capa oculta.
    h = sigmoid(x:mm(weights_input_hidden) + biases_hidden)

    -- Calculamos la salida de la capa de salida.
    y = sigmoid(h:mm(weights_hidden_output) + biases_output)

    -- Devolvemos la salida de la red neuronal.
    return y
end

-- Definimos la función de entrenamiento de la red neuronal.

function train(x, y)
    -- Calculamos la salida de la red neuronal.
    y_pred = forward_pass(x)

    -- Calculamos el error cuadrático medio entre la salida real y la salida predicha.
    loss = 1/2 * (y - y_pred)^2

    -- Calculamos el gradiente del error cuadrático medio con respecto a los pesos y sesgos de la red neuronal.
    grad_weights_input_hidden = x' * (y_pred - y)
    grad_biases_input_hidden = sum(y_pred - y, 1)

    grad_weights_hidden_output = h' * (y_pred - y)
    grad_biases_hidden_output = sum(y_pred - y, 1)

    -- Actualizamos los pesos y sesgos de la red neuronal.
    weights_input_hidden = weights_input_hidden - learning_rate * grad_weights_input_hidden
    biases_hidden = biases_hidden - learning_rate * grad_biases_input_hidden

    weights_hidden_output = weights_hidden_output - learning_rate * grad_weights_hidden_output
    biases_output = biases_output - learning_rate * grad_biases_hidden_output
end

-- Cargamos los datos de entrenamiento.

data = torch.load('data.pt')
x = data[1]
y = data[2]

-- Definimos el número de iteraciones de entrenamiento.

n_iterations = 1000

-- Definimos el tamaño del lote de entrenamiento.

batch_size = 100

-- Definimos el tipo de optimizador que se usará para entrenar la red neuronal.

optimizer = torch.optim.SGD({
    weights_input_hidden,
    biases_hidden,
    weights_hidden_output,
    biases_output
}, lr=learning_rate)

-- Entrenamos la red neuronal.

for i = 1, n_iterations do
    for j = 1, math.ceil(x:size(1) / batch_size) do
        indices = (j - 1) * batch_size + 1:j * batch_size
        x_batch = x[indices, :]
        y_batch = y[indices, :]
        optimizer:zero_grad()
        forward_pass(x_batch)
        train(x_batch, y_batch)
        optimizer:step()
    end
end

-- Guardamos la red neuronal entrenada.

torch.save(model, 'model.pt')
```

Explicación:

* **Funciones de activación:** Este código utiliza las funciones de activación sigmoide y ReLU. La función sigmoide se utiliza en la capa oculta, mientras que la función ReLU se utiliza en la capa de salida.
* **Arquitectura de la red neuronal:** Este código implementa una red neuronal de propagación hacia adelante con una capa oculta y una capa de salida. La capa oculta tiene 5 neuronas, mientras que la capa de salida tiene 2 neuronas.
* **Inicialización de los pesos y sesgos:** Los pesos y sesgos de la red neuronal se inicializan aleatoriamente.
* **Función de propagación hacia adelante:** Esta función calcula la salida de la red neuronal para una entrada dada. La salida de la capa oculta se calcula aplicando la función sigmoide a la suma ponderada de las entradas. La salida de la capa de salida se calcula aplicando la función ReLU a la suma ponderada de las salidas de la capa oculta.
* **Función de entrenamiento:** Esta función entrena la red neuronal utilizando el algoritmo de descenso del gradiente estocástico. La función calcula el error cuadrático medio entre la salida real y la salida predicha, y luego calcula el gradiente del error cuadrático medio con respecto a los pesos y sesgos de la red neuronal. Los pesos y sesgos se actualizan utilizando el gradiente y el tipo de optimizador especificado.
* **Carga de los datos de entrenamiento:** Este código carga los datos de entrenamiento desde un archivo .pt.
* **Definición del número de iteraciones de entrenamiento:** Este código define el número de iteraciones de entrenamiento en 1000.
* **Definición del tamaño del lote de entrenamiento:** Este código define el tamaño del lote de entrenamiento en 100.
* **Definición del tipo de optimizador:** Este código utiliza el optimizador SGD (Stochastic Gradient Descent) para entrenar la red neuronal.
* **Entrenamiento de la red neuronal:** Este código entrena la red neuronal durante el número especificado de iteraciones. En cada iteración, el código divide los datos de entrenamiento en lotes y entrena la red neuronal utilizando cada lote.
* **Guardado de la red neuronal entrenada:** Este código guarda la red neuronal entrenada en un archivo .pt.