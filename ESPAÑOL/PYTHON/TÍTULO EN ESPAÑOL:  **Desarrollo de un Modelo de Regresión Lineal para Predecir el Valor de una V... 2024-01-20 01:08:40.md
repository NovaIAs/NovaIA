```python
# Importación de bibliotecas
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.linear_model import LinearRegression
from sklearn.model_selection import train_test_split
from sklearn.metrics import r2_score

# Carga de datos
datos = pd.read_csv('datos.csv')

# Limpieza de datos
datos = datos.dropna()

# Creación de variables independientes y dependientes
X = datos[['variable_1', 'variable_2']]
y = datos['variable_objetivo']

# División de los datos en conjuntos de entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrenamiento del modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluación del modelo
r2 = r2_score(y_test, modelo.predict(X_test))
print('El coeficiente de determinación (R^2) es:', r2)

# Gráfica de la regresión lineal
plt.scatter(X_test['variable_1'], y_test, color='blue', label='Datos reales')
plt.plot(X_test['variable_1'], modelo.predict(X_test), color='red', label='Regresión lineal')
plt.legend()
plt.show()

# Creación de una función para predecir el valor de la variable objetivo dados los valores de las variables independientes
def predecir(variable_1, variable_2):
  return modelo.predict([[variable_1, variable_2]])

# Ejemplo de uso de la función de predicción
prediccion = predecir(10, 20)
print('La predicción del valor de la variable objetivo es:', prediccion)
```

Explicación del código:

1. Se importan las bibliotecas necesarias para el análisis de datos y la creación del modelo de regresión lineal.
2. Se cargan los datos de un archivo CSV.
3. Se realiza una limpieza de los datos para eliminar los valores faltantes.
4. Se crean las variables independientes (X) y la variable dependiente (y).
5. Se dividen los datos en conjuntos de entrenamiento y prueba.
6. Se entrena el modelo de regresión lineal usando los datos de entrenamiento.
7. Se evalúa el modelo usando el conjunto de prueba y se calcula el coeficiente de determinación (R^2).
8. Se crea una gráfica de la regresión lineal para visualizar la relación entre las variables independientes y la variable objetivo.
9. Se crea una función para predecir el valor de la variable objetivo dados los valores de las variables independientes.
10. Se utiliza la función de predicción para hacer una predicción del valor de la variable objetivo para un conjunto dado de valores de las variables independientes.