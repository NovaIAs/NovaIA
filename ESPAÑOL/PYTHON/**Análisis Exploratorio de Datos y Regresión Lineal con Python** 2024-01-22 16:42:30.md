```python
# Importación de las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Carga de los datos
datos = pd.read_csv('datos.csv')

# Preprocesamiento de los datos
datos = datos.dropna()  # Eliminación de los valores faltantes
datos = datos.astype({'edad': 'int', 'sexo': 'category'})  # Conversión de los tipos de datos

# Análisis exploratorio de los datos
print(datos.head())  # Muestra las primeras cinco filas de los datos
print(datos.describe())  # Muestra un resumen estadístico de los datos
sns.pairplot(datos)  # Gráfica de dispersión entre las variables
plt.show()  # Muestra las gráficas

# Creación del modelo de regresión lineal
modelo = linear_model.LinearRegression()
modelo.fit(X, y)  # Entrenamiento del modelo

# Evaluación del modelo de regresión lineal
print(modelo.score(X, y))  # Muestra el coeficiente de determinación R^2
print(modelo.coef_)  # Muestra los coeficientes de la regresión lineal
print(modelo.intercept_)  # Muestra el término independiente de la regresión lineal

# Predicción de nuevos valores
nuevos_datos = pd.DataFrame({'edad': [25, 30, 35], 'sexo': ['hombre', 'mujer', 'mujer']})
predicciones = modelo.predict(nuevos_datos)
print(predicciones)  # Muestra las predicciones del modelo

# Guardado del modelo
joblib.dump(modelo, 'modelo.pkl')  # Guarda el modelo en un archivo
```

Este código es un ejemplo de un análisis exploratorio de datos y de construcción de un modelo de regresión lineal en Python. El código carga los datos, los preprocesa, los analiza y crea un modelo de regresión lineal para predecir el salario de una persona en función de su edad y su sexo. El código también evalúa el modelo y hace predicciones para nuevos datos. Finalmente, el modelo se guarda en un archivo para poder utilizarlo posteriormente.