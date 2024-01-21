```python
# Este código implementa un algoritmo de aprendizaje automático para clasificar imágenes de animales.
# El código está dividido en varias funciones y clases para hacer que sea más organizado y fácil de entender.

# Importamos las librerías necesarias.
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LogisticRegression
from sklearn.metrics import accuracy_score

# Función para cargar y procesar las imágenes de animales.
def cargar_datos(ruta_imagenes, etiquetas):
    """Carga y procesa las imágenes de animales.

    Args:
        ruta_imagenes (str): Ruta de la carpeta donde se encuentran las imágenes de animales.
        etiquetas (list): Lista de las etiquetas de las imágenes de animales.

    Returns:
        tuple: Tupla que contiene las imágenes de animales y las etiquetas correspondientes.
    """

    # Cargamos las imágenes de animales.
    imagenes = []
    for etiqueta in etiquetas:
        ruta_imagenes_etiqueta = os.path.join(ruta_imagenes, etiqueta)
        imagenes_etiqueta = [plt.imread(os.path.join(ruta_imagenes_etiqueta, imagen)) for imagen in os.listdir(ruta_imagenes_etiqueta)]
        imagenes.extend(imagenes_etiqueta)

    # Convertimos las imágenes a un array de NumPy.
    imagenes = np.array(imagenes)

    # Normalizamos las imágenes.
    imagenes = imagenes / 255.0

    # Devolvemos las imágenes de animales y las etiquetas correspondientes.
    return imagenes, etiquetas

# Función para dividir los datos en conjuntos de entrenamiento y prueba.
def dividir_datos(imagenes, etiquetas, test_size=0.2):
    """Divide los datos en conjuntos de entrenamiento y prueba.

    Args:
        imagenes (np.array): Array de NumPy con las imágenes de animales.
        etiquetas (list): Lista de las etiquetas de las imágenes de animales.
        test_size (float, opcional): Tamaño del conjunto de prueba. Por defecto es 0.2.

    Returns:
        tuple: Tupla que contiene los conjuntos de entrenamiento y prueba.
    """

    # Dividimos los datos en conjuntos de entrenamiento y prueba.
    X_train, X_test, y_train, y_test = train_test_split(imagenes, etiquetas, test_size=test_size, random_state=42)

    # Devolvemos los conjuntos de entrenamiento y prueba.
    return X_train, X_test, y_train, y_test

# Función para entrenar un clasificador de imágenes de animales.
def entrenar_clasificador(X_train, y_train):
    """Entrena un clasificador de imágenes de animales.

    Args:
        X_train (np.array): Array de NumPy con las imágenes de entrenamiento.
        y_train (list): Lista de las etiquetas de las imágenes de entrenamiento.

    Returns:
        sklearn.linear_model.LogisticRegression: Clasificador de imágenes de animales entrenado.
    """

    # Creamos un clasificador de regresión logística.
    clasificador = LogisticRegression(max_iter=1000)

    # Entrenamos el clasificador con los datos de entrenamiento.
    clasificador.fit(X_train, y_train)

    # Devolvemos el clasificador entrenado.
    return clasificador

# Función para evaluar un clasificador de imágenes de animales.
def evaluar_clasificador(clasificador, X_test, y_test):
    """Evalúa un clasificador de imágenes de animales.

    Args:
        clasificador (sklearn.linear_model.LogisticRegression): Clasificador de imágenes de animales entrenado.
        X_test (np.array): Array de NumPy con las imágenes de prueba.
        y_test (list): Lista de las etiquetas de las imágenes de prueba.

    Returns:
        float: Precisión del clasificador en el conjunto de prueba.
    """

    # Predecimos las etiquetas de las imágenes de prueba.
    y_pred = clasificador.predict(X_test)

    # Calculamos la precisión del clasificador.
    precision = accuracy_score(y_test, y_pred)

    # Devolvemos la precisión del clasificador.
    return precision

# Función para guardar un clasificador de imágenes de animales.
def guardar_clasificador(clasificador, ruta_clasificador):
    """Guarda un clasificador de imágenes de animales.

    Args:
        clasificador (sklearn.linear_model.LogisticRegression): Clasificador de imágenes de animales entrenado.
        ruta_clasificador (str): Ruta donde se guardará el clasificador.
    """

    # Guardamos el clasificador en la ruta especificada.
    joblib.dump(clasificador, ruta_clasificador)

# Función para cargar un clasificador de imágenes de animales.
def cargar_clasificador(ruta_clasificador):
    """Carga un clasificador de imágenes de animales.

    Args:
        ruta_clasificador (str): Ruta donde se encuentra el clasificador.

    Returns:
        sklearn.linear_model.LogisticRegression: Clasificador de imágenes de animales entrenado.
    """

    # Cargamos el clasificador de la ruta especificada.
    clasificador = joblib.load(ruta_clasificador)

    # Devolvemos el clasificador cargado.
    return clasificador

# Función para clasificar una imagen de animal.
def clasificar_imagen(imagen, clasificador):
    """Clasifica una imagen de animal.

    Args:
        imagen (np.array): Array de NumPy con la imagen de animal.
        clasificador (sklearn.linear_model.LogisticRegression): Clasificador de imágenes de animales entrenado.

    Returns:
        str: Etiqueta de la imagen de animal clasificada.
    """

    # Predecimos la etiqueta de la imagen de animal.
    etiqueta = clasificador.predict([imagen])[0]

    # Devolvemos la etiqueta de la imagen de animal clasificada.
    return etiqueta

# Cargamos los datos de las imágenes de animales.
ruta_imagenes = 'ruta/a/las/imagenes/de/animales'
etiquetas = ['perro', 'gato', 'conejo', 'hamster']
X, y = cargar_datos(ruta_imagenes, etiquetas)

# Dividimos los datos en conjuntos de entrenamiento y prueba.
X_train, X_test, y_train, y_test = dividir_datos(X, y)

# Entrenamos el clasificador de imágenes de animales.
clasificador = entrenar_clasificador(X_train, y_train)

# Evaluamos el clasificador de imágenes de animales.
precision = evaluar_clasificador(clasificador, X_test, y_test)
print('Precisión del clasificador:', precision)

# Guardamos el clasificador de imágenes de animales.
ruta_clasificador = 'ruta/al/clasificador/de/imagenes/de/animales'
guardar_clasificador(clasificador, ruta_clasificador)

# Cargamos el clasificador de imágenes de animales.
clasificador = cargar_clasificador(ruta_clasificador)

# Clasificamos una imagen de animal.
imagen = plt.imread('ruta/a/la/imagen/de/animal.jpg')
etiqueta = clasificar_imagen(imagen, clasificador)
print('Etiqueta de la imagen de animal clasificada:', etiqueta)
```

Este código es un ejemplo de cómo se puede utilizar la librería Scikit-Learn para crear un clasificador de imágenes de animales. El código está dividido en varias funciones y clases para hacer que sea más organizado y fácil de entender.

El código comienza cargando los datos de las imágenes de animales. Los datos se dividen en conjuntos de entrenamiento y prueba. El conjunto de entrenamiento se utiliza para entrenar el clasificador y el conjunto de prueba se utiliza para evaluar el clasificador.

El siguiente paso es entrenar el clasificador de imágenes de animales. El clasificador se entrena utilizando el conjunto de entrenamiento.

Una vez que el clasificador está entrenado, se evalúa utilizando el conjunto de prueba. La evaluación se realiza calculando la precisión del clasificador.

El último paso es guardar el clasificador entrenado. El clasificador se guarda en un archivo de formato pickle.

El código también incluye una función para cargar un clasificador entrenado. La función se utiliza para cargar el clasificador que se guardó anteriormente.

Por último, el código incluye una función para clasificar una imagen de animal. La función se utiliza para clasificar una imagen de animal utilizando el clasificador entrenado.