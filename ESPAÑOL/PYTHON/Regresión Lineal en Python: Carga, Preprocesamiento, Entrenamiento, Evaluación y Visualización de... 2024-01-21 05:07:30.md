```python
# Importamos las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos los datos de un archivo CSV
datos = pd.read_csv('datos.csv')

# Preprocesamos los datos
datos = datos.dropna()
datos = datos.drop_duplicates()

# Creamos un modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(datos[['x']], datos['y'])

# Hacemos predicciones con el modelo
predicciones = modelo.predict(datos[['x']])

# Evaluamos el modelo
print('MAE:', mean_absolute_error(datos['y'], predicciones))
print('MSE:', mean_squared_error(datos['y'], predicciones))
print('RMSE:', np.sqrt(mean_squared_error(datos['y'], predicciones)))

# Visualizamos los resultados
plt.scatter(datos['x'], datos['y'])
plt.plot(datos['x'], predicciones, color='red')
plt.show()

# Guardamos el modelo en un archivo
joblib.dump(modelo, 'modelo.pkl')

# Cargamos el modelo de un archivo
modelo_cargado = joblib.load('modelo.pkl')

# Hacemos predicciones con el modelo cargado
predicciones_cargadas = modelo_cargado.predict(datos[['x']])

# Evaluamos el modelo cargado
print('MAE:', mean_absolute_error(datos['y'], predicciones_cargadas))
print('MSE:', mean_squared_error(datos['y'], predicciones_cargadas))
print('RMSE:', np.sqrt(mean_squared_error(datos['y'], predicciones_cargadas)))
```

Este código es un ejemplo de cómo crear un modelo de regresión lineal en Python. El código se puede dividir en las siguientes partes:

* **Carga de los datos:** El código carga los datos de un archivo CSV utilizando la librería `pandas`.
* **Preprocesamiento de los datos:** El código limpia los datos eliminando los valores nulos y los duplicados.
* **Creación del modelo:** El código crea un modelo de regresión lineal utilizando la librería `scikit-learn`.
* **Entrenamiento del modelo:** El código entrena el modelo con los datos cargados.
* **Evaluación del modelo:** El código evalúa el modelo utilizando métricas como el error absoluto medio (MAE), el error cuadrático medio (MSE) y la raíz del error cuadrático medio (RMSE).
* **Visualización de los resultados:** El código visualiza los resultados del modelo mediante un gráfico de dispersión.
* **Guardado del modelo:** El código guarda el modelo en un archivo utilizando la librería `joblib`.
* **Carga del modelo:** El código carga el modelo de un archivo utilizando la librería `joblib`.
* **Predicciones con el modelo cargado:** El código realiza predicciones con el modelo cargado utilizando los datos cargados.
* **Evaluación del modelo cargado:** El código evalúa el modelo cargado utilizando métricas como el error absoluto medio (MAE), el error cuadrático medio (MSE) y la raíz del error cuadrático medio (RMSE).