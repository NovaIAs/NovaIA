```
#!/bin/bash

# Este código crea una red neuronal artificial simple usando la biblioteca TensorFlow.

# Importar las dependencias necesarias.
import tensorflow as tf

# Crear un modelo de red neuronal artificial.
modelo = tf.keras.Sequential([
  tf.keras.layers.Dense(units=10, activation='relu', input_shape=(784,)),
  tf.keras.layers.Dense(units=10, activation='relu'),
  tf.keras.layers.Dense(units=10, activation='softmax')
])

# Compilar el modelo.
modelo.compile(optimizer='adam', loss='sparse_categorical_crossentropy', metrics=['accuracy'])

# Entrenar el modelo.
modelo.fit(datos_entrenamiento, etiquetas_entrenamiento, epochs=10)

# Evaluar el modelo.
pérdida, precisión = modelo.evaluate(datos_prueba, etiquetas_prueba)

# Mostrar la precisión del modelo.
print("Precisión:", precisión)

# Guardar el modelo.
modelo.save('mi_modelo.h5')
```

Explicación del código:

* La primera línea importa la biblioteca TensorFlow.
* La segunda línea crea un modelo de red neuronal artificial simple usando la clase `tf.keras.Sequential`. El modelo tiene tres capas: una capa de entrada con 784 neuronas, una capa oculta con 10 neuronas y una capa de salida con 10 neuronas.
* La tercera línea compila el modelo. El optimizador es 'adam', la función de pérdida es 'sparse_categorical_crossentropy' y la métrica es 'accuracy'.
* La cuarta línea entrena el modelo usando los datos de entrenamiento y las etiquetas de entrenamiento. El modelo se entrena durante 10 épocas.
* La quinta línea evalúa el modelo usando los datos de prueba y las etiquetas de prueba. La función `evaluate()` devuelve la pérdida y la precisión del modelo.
* La sexta línea muestra la precisión del modelo.
* La séptima línea guarda el modelo en un archivo llamado 'mi_modelo.h5'.