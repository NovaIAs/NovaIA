```python
# Imports necesarios
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Carga de datos
datos = pd.read_csv('datos.csv')

# Exploración de datos
print(datos.head())
print(datos.info())

# Preparación de datos
# Crear variables independientes y dependientes
X = datos.drop('variable_dependiente', axis=1)
y = datos['variable_dependiente']

# Dividir datos en entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrenamiento del modelo
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluación del modelo
y_pred = modelo.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print("Error cuadrático medio:", mse)
print("Coeficiente de determinación:", r2)

# Visualización de resultados
plt.scatter(y_test, y_pred)
plt.xlabel('Valor real')
plt.ylabel('Valor predicho')
plt.title('Comparación entre valores reales y predichos')
plt.show()

# Guardar el modelo entrenado
import pickle

filename = 'modelo_entrenado.sav'
pickle.dump(modelo, open(filename, 'wb'))

# Carga del modelo entrenado
loaded_model = pickle.load(open(filename, 'rb'))

# Predicción de nuevos datos
nuevos_datos = pd.DataFrame({
    'variable_1': [1, 2, 3],
    'variable_2': [4, 5, 6]
})

nuevos_y_pred = loaded_model.predict(nuevos_datos)
print("Predicciones para los nuevos datos:", nuevos_y_pred)
```

Explicación:

* El código comienza importando las bibliotecas necesarias, incluyendo Pandas para el manejo de datos, Numpy para operaciones matemáticas, Matplotlib para la visualización de datos y Scikit-Learn para el aprendizaje automático.
* A continuación, se cargan los datos de un archivo CSV utilizando Pandas.
* Después, se realiza una exploración de los datos, imprimiendo las primeras filas y la información general de los mismos, como el tipo de datos de cada columna y el número de valores faltantes.
* A continuación, se preparan los datos para el entrenamiento del modelo. Esto incluye dividir los datos en variables independientes (X) y dependientes (y), y dividir los datos en conjuntos de entrenamiento y prueba.
* El modelo de regresión lineal se entrena utilizando los datos de entrenamiento.
* Se evalúa el rendimiento del modelo utilizando el conjunto de datos de prueba, calculando el error cuadrático medio y el coeficiente de determinación.
* Se visualiza la relación entre los valores reales y los valores predichos en un diagrama de dispersión.
* Finalmente, el modelo entrenado se guarda en un archivo utilizando la biblioteca Pickle.

Este código es complejo y diferenciado porque incluye varias etapas de preparación de datos, entrenamiento, evaluación y visualización de resultados, además de utilizar varias bibliotecas de Python para realizar estas tareas.