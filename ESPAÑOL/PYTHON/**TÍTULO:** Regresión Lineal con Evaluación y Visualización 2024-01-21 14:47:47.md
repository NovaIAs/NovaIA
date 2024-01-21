```python
# Importación de las bibliotecas necesarias.
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import sklearn.linear_model as lm
import sklearn.model_selection as ms
import sklearn.metrics as me

# Carga de los datos.
datos = pd.read_csv('datos.csv')

# Preprocesamiento de los datos.
X = datos.drop('y', axis=1)
y = datos['y']

# División de los datos en conjuntos de entrenamiento y prueba.
X_train, X_test, y_train, y_test = ms.train_test_split(X, y, test_size=0.2, random_state=42)

# Creación del modelo.
modelo = lm.LinearRegression()

# Entrenamiento del modelo.
modelo.fit(X_train, y_train)

# Evaluación del modelo.
y_pred = modelo.predict(X_test)
mse = me.mean_squared_error(y_test, y_pred)
rmse = np.sqrt(mse)
r2 = me.r2_score(y_test, y_pred)

# Impresión de los resultados.
print('El MSE del modelo es:', mse)
print('El RMSE del modelo es:', rmse)
print('El R^2 del modelo es:', r2)

# Gráfica de los datos y del modelo.
plt.scatter(X_test['x'], y_test, color='blue', label='Datos')
plt.plot(X_test['x'], y_pred, color='red', label='Modelo')
plt.xlabel('x')
plt.ylabel('y')
plt.legend()
plt.show()
```

Explicación del código:

* El código comienza importando las bibliotecas necesarias: `numpy` para el manejo de datos numéricos, `matplotlib.pyplot` para la generación de gráficos, `pandas` para el manejo de datos tabulares, `sklearn.linear_model` para la creación del modelo de regresión lineal, `sklearn.model_selection` para la división de los datos en conjuntos de entrenamiento y prueba, y `sklearn.metrics` para la evaluación del modelo.
* A continuación, se cargan los datos desde un archivo CSV utilizando la función `pd.read_csv()`.
* Los datos se preprocesan separando las variables independientes (`X`) de la variable dependiente (`y`).
* Se dividen los datos en conjuntos de entrenamiento y prueba utilizando la función `ms.train_test_split()`. El conjunto de entrenamiento se utilizará para entrenar el modelo, mientras que el conjunto de prueba se utilizará para evaluar el modelo.
* Se crea el modelo de regresión lineal utilizando la clase `lm.LinearRegression()`.
* Se entrena el modelo utilizando la función `fit()`.
* Se evalúa el modelo utilizando la función `predict()` para predecir los valores de `y` para el conjunto de prueba. A continuación, se calculan el MSE, el RMSE y el R^2 utilizando las funciones `me.mean_squared_error()`, `np.sqrt()` y `me.r2_score()` respectivamente.
* Se imprimen los resultados de la evaluación del modelo.
* Finalmente, se genera una gráfica de los datos y del modelo utilizando la función `plt.scatter()` para los datos y `plt.plot()` para el modelo.