```python
# Importamos las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos el dataset
datos = pd.read_csv('datos.csv')

# Exploramos los datos
print(datos.head())
print(datos.describe())

# Creamos un gráfico de dispersión para ver la relación entre las variables
sns.scatterplot(x="variable_1", y="variable_2", data=datos)
plt.show()

# Creamos un modelo de regresión lineal para predecir la variable_2 a partir de la variable_1
modelo = sklearn.linear_model.LinearRegression()
modelo.fit(datos[['variable_1']], datos['variable_2'])

# Evaluamos el modelo
print(modelo.score(datos[['variable_1']], datos['variable_2']))

# Hacemos predicciones
predicciones = modelo.predict(datos[['variable_1']])

# Creamos un gráfico para comparar las predicciones con los valores reales
plt.scatter(datos['variable_2'], predicciones)
plt.plot(np.linspace(0, 100, 100), np.linspace(0, 100, 100), 'r')
plt.xlabel('Valores reales')
plt.ylabel('Predicciones')
plt.show()
```

Este código es un ejemplo de cómo utilizar las bibliotecas de Python para explorar y analizar datos, crear modelos de machine learning y hacer predicciones. El código está dividido en varias secciones:

* **Importación de las bibliotecas necesarias:** En esta sección se importan las bibliotecas de Python que se utilizarán en el código.
* **Carga del dataset:** En esta sección se carga el dataset que se utilizará para el análisis.
* **Exploración de los datos:** En esta sección se exploran los datos utilizando el método `head()` de Pandas para mostrar las primeras filas del dataset y el método `describe()` para mostrar estadísticas descriptivas de las variables.
* **Creación de un gráfico de dispersión:** En esta sección se crea un gráfico de dispersión para visualizar la relación entre las variables.
* **Creación de un modelo de regresión lineal:** En esta sección se crea un modelo de regresión lineal para predecir la variable_2 a partir de la variable_1.
* **Evaluación del modelo:** En esta sección se evalúa el modelo utilizando el método `score()` de Scikit-Learn.
* **Realización de predicciones:** En esta sección se realizan predicciones utilizando el método `predict()` del modelo.
* **Creación de un gráfico para comparar las predicciones con los valores reales:** En esta sección se crea un gráfico para comparar las predicciones con los valores reales.