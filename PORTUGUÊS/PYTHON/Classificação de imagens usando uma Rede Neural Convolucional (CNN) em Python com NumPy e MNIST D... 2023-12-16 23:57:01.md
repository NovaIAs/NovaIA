Claro! Aqui está um código complexo em Python que usa a biblioteca NumPy para criar uma rede neural convolucional (CNN) para classificar imagens usando o conjunto de dados MNIST. A CNN possui várias camadas de convolução, ativação e pooling, seguidas por camadas totalmente conectadas e uma camada de saída para classificação.

```python
import numpy as np
import tensorflow as tf
from tensorflow.keras import layers, models
from tensorflow.keras.datasets import mnist

# Carrega o conjunto de dados MNIST
(x_train, y_train), (x_test, y_test) = mnist.load_data()

# Pré-processamento dos dados
x_train = x_train.reshape(-1, 28, 28, 1).astype("float32") / 255.0
x_test = x_test.reshape(-1, 28, 28, 1).astype("float32") / 255.0
y_train = tf.keras.utils.to_categorical(y_train, num_classes=10)
y_test = tf.keras.utils.to_categorical(y_test, num_classes=10)

# Constrói a arquitetura da rede neural convolucional
model = models.Sequential()
model.add(layers.Conv2D(32, (3, 3), activation='relu', input_shape=(28, 28, 1)))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.MaxPooling2D((2, 2)))
model.add(layers.Conv2D(64, (3, 3), activation='relu'))
model.add(layers.Flatten())
model.add(layers.Dense(64, activation='relu'))
model.add(layers.Dense(10, activation='softmax'))

# Compila e treina a rede neural
model.compile(optimizer='adam',
              loss='categorical_crossentropy',
              metrics=['accuracy'])
model.fit(x_train, y_train, epochs=10, batch_size=128, validation_data=(x_test, y_test))

# Avalia o desempenho do modelo
test_loss, test_acc = model.evaluate(x_test, y_test)
print(f"Acurácia do modelo: {test_acc}")
```

Este código utiliza o conjunto de dados MNIST, que consiste em imagens de dígitos escritos à mão. Ele carrega os dados, normaliza-os, cria a arquitetura da CNN usando a biblioteca Keras com camadas convolucionais, ativação ReLU, pooling e camadas totalmente conectadas. Em seguida, ele compila o modelo e treina a CNN usando o otimizador Adam e a função de perda de entropia cruzada categórica. Após o treinamento, o modelo é avaliado em um conjunto de teste e a acurácia é exibida.