```python
# Importamos las librerías necesarias.
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargamos el conjunto de datos.
data = pd.read_csv('data.csv')

# Dividimos el conjunto de datos en entrenamiento y prueba.
X_train, X_test, y_train, y_test = train_test_split(data.drop('y', axis=1), data['y'], test_size=0.2, random_state=42)

# Creamos el modelo de regresión lineal.
model = LinearRegression()

# Entrenamos el modelo.
model.fit(X_train, y_train)

# Evaluamos el modelo.
y_pred = model.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Imprimimos los resultados.
print('MSE:', mse)
print('R2:', r2)

# Visualizamos los resultados.
plt.scatter(y_test, y_pred)
plt.xlabel('Y real')
plt.ylabel('Y predicha')
plt.show()
```

Este código es un ejemplo de cómo crear un modelo de regresión lineal en Python. El código está explicado en detalle a continuación:

1. **Importamos las librerías necesarias.**
   - `numpy`: Esta librería nos permite trabajar con matrices y vectores numéricos.
   - `matplotlib.pyplot`: Esta librería nos permite generar gráficos.
   - `pandas`: Esta librería nos permite trabajar con conjuntos de datos.
   - `sklearn.model_selection`: Esta librería nos permite dividir el conjunto de datos en entrenamiento y prueba.
   - `sklearn.linear_model`: Esta librería nos permite crear modelos de regresión lineal.
   - `sklearn.metrics`: Esta librería nos permite evaluar el rendimiento de los modelos de regresión lineal.

2. **Cargamos el conjunto de datos.**
   - Utilizamos la función `pd.read_csv()` para cargar el conjunto de datos desde un archivo CSV. El conjunto de datos debe tener una columna con el nombre `y` que contenga los valores de la variable dependiente.

3. **Dividimos el conjunto de datos en entrenamiento y prueba.**
   - Utilizamos la función `train_test_split()` para dividir el conjunto de datos en entrenamiento y prueba. La función toma como argumentos el conjunto de datos, la variable dependiente y el tamaño de la muestra de prueba.

4. **Creamos el modelo de regresión lineal.**
   - Utilizamos la función `LinearRegression()` para crear un modelo de regresión lineal.

5. **Entrenamos el modelo.**
   - Utilizamos la función `fit()` para entrenar el modelo con el conjunto de datos de entrenamiento.

6. **Evaluamos el modelo.**
   - Utilizamos la función `mean_squared_error()` para calcular el error cuadrático medio (MSE) entre los valores reales y los valores predichos.
   - Utilizamos la función `r2_score()` para calcular el coeficiente de determinación (R2) entre los valores reales y los valores predichos.

7. **Imprimimos los resultados.**
   - Imprimimos el MSE y el R2 para mostrar el rendimiento del modelo.

8. **Visualizamos los resultados.**
   - Utilizamos la función `scatter()` para generar un gráfico de dispersión que muestra los valores reales y los valores predichos.