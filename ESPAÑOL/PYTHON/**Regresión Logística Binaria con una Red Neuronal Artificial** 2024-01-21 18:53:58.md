```python
# Importamos las bibliotecas necesarias
import random
import math
import numpy as np
import matplotlib.pyplot as plt

# Definimos la función que calcula el valor de la función logística
def logistica(x):
  return 1 / (1 + math.exp(-x))

# Definimos la función que calcula el error cuadrático medio
def error_cuadratico_medio(y_pred, y_true):
  return np.mean((y_pred - y_true)**2)

# Definimos la clase que representa a la red neuronal
class RedNeuronal:
  def __init__(self, capas, activaciones):
    # Inicializamos las capas de la red neuronal
    self.capas = capas

    # Inicializamos las activaciones de la red neuronal
    self.activaciones = activaciones

    # Inicializamos los pesos de la red neuronal
    self.pesos = []
    for i in range(len(capas) - 1):
      self.pesos.append(np.random.randn(capas[i], capas[i + 1]))

    # Inicializamos los sesgos de la red neuronal
    self.sesgos = []
    for i in range(len(capas) - 1):
      self.sesgos.append(np.zeros((1, capas[i + 1])))

  # Definimos el método que realiza una predicción
  def predecir(self, X):
    # Propagamos la entrada a través de la red neuronal
    for i in range(len(self.capas) - 1):
      X = np.dot(X, self.pesos[i]) + self.sesgos[i]
      X = self.activaciones[i](X)

    # Devolvemos la salida de la red neuronal
    return X

  # Definimos el método que entrena la red neuronal
  def entrenar(self, X, y, epochs, batch_size, learning_rate):
    # Dividimos el conjunto de datos en lotes
    batches = [X[i:i + batch_size] for i in range(0, X.shape[0], batch_size)]

    # Iteramos sobre las épocas
    for epoch in range(epochs):
      # Iteramos sobre los lotes
      for batch in batches:
        # Propagamos la entrada a través de la red neuronal
        y_pred = self.predecir(batch)

        # Calculamos el error cuadrático medio
        error = error_cuadratico_medio(y_pred, y)

        # Calculamos el gradiente del error con respecto a los pesos y sesgos
        gradiente_pesos = []
        gradiente_sesgos = []
        for i in range(len(self.capas) - 1):
          gradiente_pesos.append(np.dot(batch.T, error))
          gradiente_sesgos.append(np.sum(error, axis=0, keepdims=True))

        # Actualizamos los pesos y sesgos
        for i in range(len(self.capas) - 1):
          self.pesos[i] -= learning_rate * gradiente_pesos[i]
          self.sesgos[i] -= learning_rate * gradiente_sesgos[i]

# Creamos una red neuronal
red_neuronal = RedNeuronal([2, 3, 1], [logistica])

# Entrenamos la red neuronal
red_neuronal.entrenar(X, y, 1000, 32, 0.01)

# Evaluamos la red neuronal
y_pred = red_neuronal.predecir(X)
error = error_cuadratico_medio(y_pred, y)
print("Error cuadrático medio:", error)

# Mostramos los resultados
plt.scatter(X[:, 0], X[:, 1], c=y)
plt.plot(X[:, 0], y_pred, color='red')
plt.show()
```

Este código es un ejemplo de una red neuronal artificial que realiza una regresión logística binaria. La red neuronal consta de una capa de entrada de dos neuronas, una capa oculta de tres neuronas y una capa de salida de una neurona. La función de activación de la capa oculta es la función logística y la función de activación de la capa de salida es la identidad.

El código también incluye una función para calcular el error cuadrático medio, una función para entrenar la red neuronal y una función para evaluarla.

El código se utiliza para entrenar la red neuronal en un conjunto de datos de dos características y una etiqueta binaria. La red neuronal se entrena durante 1000 épocas, con un tamaño de lote de 32 y una tasa de aprendizaje de 0,01.

Después de entrenar la red neuronal, se evalúa en el conjunto de datos de prueba y se muestra el error cuadrático medio. También se muestra una gráfica que representa la distribución de los datos y la línea de decisión de la red neuronal.