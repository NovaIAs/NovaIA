```python
# Importación de librerías necesarias
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Carga de los datos
datos = pd.read_csv('datos.csv')

# Preprocesamiento de los datos
datos = datos.dropna()
datos['Fecha'] = pd.to_datetime(datos['Fecha'])
datos['Año'] = datos['Fecha'].dt.year
datos['Mes'] = datos['Fecha'].dt.month
datos['Día'] = datos['Fecha'].dt.day

# División de los datos en entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(
    datos[['Año', 'Mes', 'Día']],
    datos['Valor'],
    test_size=0.2,
    random_state=42
)

# Entrenamiento del modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Predicción de los valores de prueba
y_pred = modelo.predict(X_test)

# Evaluación del modelo
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print('MSE:', mse)
print('R2:', r2)

# Visualización de los resultados
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Valor real')
plt.ylabel('Valor predicho')
plt.title('Regresión lineal')
plt.show()
```

Explicación del código:

1. **Importación de librerías:** Se importan las librerías necesarias para el análisis de datos y el modelado de regresión lineal.

2. **Carga de los datos:** Se carga el conjunto de datos desde un archivo CSV.

3. **Preprocesamiento de los datos:** Se eliminan los valores faltantes, se convierten las fechas a fechas y se extraen las columnas de año, mes y día.

4. **División de los datos en entrenamiento y prueba:** Se divide el conjunto de datos en dos conjuntos: entrenamiento y prueba. El conjunto de entrenamiento se utilizará para entrenar el modelo y el conjunto de prueba se utilizará para evaluar el modelo.

5. **Entrenamiento del modelo de regresión lineal:** Se crea un modelo de regresión lineal y se entrena con el conjunto de entrenamiento.

6. **Predicción de los valores de prueba:** Se utilizan los datos de prueba para predecir los valores de la variable objetivo.

7. **Evaluación del modelo:** Se evalúa el rendimiento del modelo calculando el error cuadrático medio (MSE) y el coeficiente de determinación (R2).

8. **Visualización de los resultados:** Se visualiza la relación entre los valores reales y los valores predichos utilizando un gráfico de dispersión.