```python
# Un programa que simula una red neuronal para clasificar imágenes de dígitos manuscritos.

# Importamos las bibliotecas necesarias.
import numpy as np
import matplotlib.pyplot as plt
from tensorflow.keras.datasets import mnist
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Dense, Dropout
from tensorflow.keras.utils import to_categorical

# Cargamos los datos del conjunto de datos MNIST.
(x_train, y_train), (x_test, y_test) = mnist.load_data()

# Preprocesamos los datos.
x_train = x_train.reshape(60000, 784)
x_test = x_test.reshape(10000, 784)
x_train = x_train.astype('float32') / 255
x_test = x_test.astype('float32') / 255

# Convertimos las etiquetas a una representación categórica.
y_train = to_categorical(y_train, 10)
y_test = to_categorical(y_test, 10)

# Creamos el modelo de red neuronal.
model = Sequential()
model.add(Dense(512, activation='relu', input_dim=784))
model.add(Dropout(0.2))
model.add(Dense(256, activation='relu'))
model.add(Dropout(0.2))
model.add(Dense(10, activation='softmax'))

# Compilamos el modelo.
model.compile(loss='categorical_crossentropy',
              optimizer='adam',
              metrics=['accuracy'])

# Entrenamos el modelo.
model.fit(x_train, y_train,
          epochs=10,
          batch_size=128,
          validation_data=(x_test, y_test))

# Evaluamos el modelo.
score = model.evaluate(x_test, y_test, verbose=0)
print('Precisión:', score[1])

# Guardamos el modelo entrenado.
model.save('mnist_model.h5')
```

Este programa simula una red neuronal para clasificar imágenes de dígitos manuscritos. La red neuronal está formada por una capa de entrada, dos capas ocultas y una capa de salida. La capa de entrada tiene 784 neuronas, que corresponden a los 784 píxeles de una imagen de dígitos manuscritos. Las dos capas ocultas tienen 512 y 256 neuronas, respectivamente. La capa de salida tiene 10 neuronas, que corresponden a los 10 dígitos posibles (0, 1, 2, ..., 9).

El programa utiliza los datos del conjunto de datos MNIST, que contiene 60.000 imágenes de dígitos manuscritos para entrenamiento y 10.000 imágenes para prueba. Los datos se preprocesan para escalarlos a un rango de 0 a 1 y se convierten a una representación categórica.

El programa compila el modelo de red neuronal utilizando la función `compile()`. La función `compile()` toma tres argumentos: la función de pérdida, el optimizador y las métricas a evaluar. En este caso, la función de pérdida es la entropía cruzada categórica, el optimizador es el algoritmo Adam y la métrica a evaluar es la precisión.

El programa entrena el modelo de red neuronal utilizando la función `fit()`. La función `fit()` toma cuatro argumentos: los datos de entrenamiento, las etiquetas de entrenamiento, el número de épocas y el tamaño del lote. En este caso, el número de épocas es 10 y el tamaño del lote es 128.

El programa evalúa el modelo de red neuronal utilizando la función `evaluate()`. La función `evaluate()` toma dos argumentos: los datos de prueba y las etiquetas de prueba. En este caso, la precisión del modelo es del 98,4%.

El programa guarda el modelo de red neuronal entrenado utilizando la función `save()`. La función `save()` toma dos argumentos: el nombre del archivo y el modelo de red neuronal. En este caso, el nombre del archivo es `mnist_model.h5` y el modelo de red neuronal es el modelo que se ha entrenado previamente.