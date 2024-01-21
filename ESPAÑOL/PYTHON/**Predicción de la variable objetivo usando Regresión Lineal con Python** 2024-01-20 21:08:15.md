```python
# Importa las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error

# Carga los datos
datos = pd.read_csv('datos.csv')

# Explora los datos
print(datos.head())
print(datos.info())
print(datos.describe())

# Crea variables independientes y dependientes
X = datos[['variable_1', 'variable_2', 'variable_3']]
y = datos['variable_objetivo']

# Divide los datos en conjuntos de entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrena el modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evalúa el modelo de regresión lineal
y_pred = modelo.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
print('El error cuadrático medio es:', mse)

# Visualiza los resultados
plt.scatter(y_test, y_pred)
plt.xlabel('Variable objetivo real')
plt.ylabel('Variable objetivo predicha')
plt.title('Comparación de la variable objetivo real y la predicha')
plt.show()

# Guarda el modelo entrenado
import pickle
filename = 'modelo.pkl'
pickle.dump(modelo, open(filename, 'wb'))

# Carga el modelo entrenado
filename = 'modelo.pkl'
modelo = pickle.load(open(filename, 'rb'))
```

Explicación del código:

* El código comienza importando las bibliotecas necesarias, incluyendo NumPy, Pandas, Matplotlib, Seaborn y Scikit-Learn.
* Luego carga los datos de un archivo CSV utilizando la función `pd.read_csv()`.
* A continuación, explora los datos utilizando las funciones `head()`, `info()` y `describe()` de Pandas.
* Divide los datos en conjuntos de entrenamiento y prueba utilizando la función `train_test_split()` de Scikit-Learn.
* Entrena el modelo de regresión lineal utilizando la función `fit()` del modelo.
* Evalúa el modelo de regresión lineal utilizando la función `mean_squared_error()` de Scikit-Learn y la función `predict()` del modelo.
* Visualiza los resultados utilizando la biblioteca Matplotlib.
* Guarda el modelo entrenado utilizando la función `pickle.dump()`.
* Carga el modelo entrenado utilizando la función `pickle.load()`.