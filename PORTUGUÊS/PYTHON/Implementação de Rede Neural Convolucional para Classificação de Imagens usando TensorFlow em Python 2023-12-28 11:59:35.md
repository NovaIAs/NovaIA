Claro! Aqui está um código em Python que implementa um algoritmo de aprendizado de máquina para classificar imagens usando a biblioteca TensorFlow. Este código utiliza uma rede neural convolucional (CNN) para realizar a classificação.

```python
import tensorflow as tf
from tensorflow.keras.datasets import cifar10
from tensorflow.keras.models import Sequential
from tensorflow.keras.layers import Conv2D, MaxPooling2D, Flatten, Dense
from tensorflow.keras.optimizers import Adam
from tensorflow.keras.utils import to_categorical

# Carregando o conjunto de dados CIFAR-10
(x_train, y_train), (x_test, y_test) = cifar10.load_data()

# Pré-processamento dos dados
x_train = x_train.astype('float32') / 255
x_test = x_test.astype('float32') / 255
y_train = to_categorical(y_train, num_classes=10)
y_test = to_categorical(y_test, num_classes=10)

# Criando o modelo CNN
model = Sequential()
model.add(Conv2D(32, (3, 3), activation='relu', padding='same', input_shape=(32, 32, 3)))
model.add(MaxPooling2D((2, 2)))
model.add(Conv2D(64, (3, 3), activation='relu', padding='same'))
model.add(MaxPooling2D((2, 2)))
model.add(Conv2D(64, (3, 3), activation='relu', padding='same'))
model.add(Flatten())
model.add(Dense(64, activation='relu'))
model.add(Dense(10, activation='softmax'))

# Compilando o modelo
model.compile(optimizer=Adam(), loss='categorical_crossentropy', metrics=['accuracy'])

# Treinando o modelo
model.fit(x_train, y_train, batch_size=64, epochs=10, validation_data=(x_test, y_test))

# Avaliando o modelo
loss, accuracy = model.evaluate(x_test, y_test)
print(f'Acurácia do modelo: {accuracy * 100}%')
```

Explicação do código:

1. Importamos as bibliotecas necessárias, incluindo o TensorFlow para construir e treinar a CNN.
2. Carregamos o conjunto de dados CIFAR-10, que contém imagens de 10 classes diferentes.
3. Pré-processamos os dados, normalizando as imagens e realizando a codificação one-hot das classes.
4. Criamos o modelo da CNN utilizando a classe `Sequential` do Keras. Adicionamos camadas de convolução, max pooling e camadas densas para construir a arquitetura da rede.
5. Compilamos o modelo, definindo o otimizador Adam e a função de perda como "categorical_crossentropy".
6. Treinamos o modelo usando o método `fit`, especificando o tamanho do lote, o número de épocas e os dados de validação.
7. Avaliamos o modelo usando o conjunto de teste e exibimos a acurácia alcançada.

Esse código utiliza uma arquitetura CNN padrão para classificação de imagens. É importante ressaltar que a complexidade do código pode variar dependendo do problema e do modelo utilizado.