```python
# Importamos las librerías necesarias.
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import sklearn
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargamos el conjunto de datos.
data = pd.read_csv('datos.csv')

# Separamos los datos en características y etiquetas.
X = data.drop('etiqueta', axis=1)
y = data['etiqueta']

# Dividimos los datos en entrenamiento y prueba.
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Creamos un modelo de regresión lineal.
model = LinearRegression()

# Entrenamos el modelo.
model.fit(X_train, y_train)

# Hacemos predicciones en los datos de prueba.
y_pred = model.predict(X_test)

# Evaluamos el rendimiento del modelo.
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

print(f'MSE: {mse}')
print(f'R^2: {r2}')

# Mostramos los coeficientes del modelo.
print(f'Coeficientes: {model.coef_}')

# Mostramos la intersección del modelo.
print(f'Intersección: {model.intercept_}')

# Mostramos una gráfica con los datos reales y los datos predichos.
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Datos reales')
plt.ylabel('Datos predichos')
plt.show()
```

Explicación del código:

* Importamos las librerías necesarias.
* Cargamos el conjunto de datos.
* Separamos los datos en características y etiquetas.
* Dividimos los datos en entrenamiento y prueba.
* Creamos un modelo de regresión lineal.
* Entrenamos el modelo.
* Hacemos predicciones en los datos de prueba.
* Evaluamos el rendimiento del modelo.
* Mostramos los coeficientes del modelo.
* Mostramos la intersección del modelo.
* Mostramos una gráfica con los datos reales y los datos predichos.