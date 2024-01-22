```python
# Importamos las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import sklearn.model_selection as model_selection
import sklearn.linear_model as linear_model
import sklearn.metrics as metrics

# Cargamos los datos
datos = pd.read_csv('datos.csv')

# Preprocesamos los datos
datos.dropna(inplace=True)
datos.drop_duplicates(inplace=True)
datos['edad'] = datos['edad'].astype('int64')

# Creamos las variables dependiente e independientes
X = datos.drop('clase', axis=1)
y = datos['clase']

# Dividimos los datos en train y test
X_train, X_test, y_train, y_test = model_selection.train_test_split(X, y, test_size=0.2)

# Entrenamos el modelo
modelo = linear_model.LogisticRegression()
modelo.fit(X_train, y_train)

# Evaluamos el modelo
y_pred = modelo.predict(X_test)
print("Precisión:", metrics.accuracy_score(y_test, y_pred))
print("Recall:", metrics.recall_score(y_test, y_pred))
print("F1-score:", metrics.f1_score(y_test, y_pred))

# Graficamos los resultados
plt.scatter(X_test['edad'], y_test, label='Datos reales')
plt.scatter(X_test['edad'], y_pred, label='Datos predichos')
plt.xlabel('Edad')
plt.ylabel('Clase')
plt.legend()
plt.show()
```

Explicación del código:

1. Importamos las librerías necesarias.
2. Cargamos los datos del fichero 'datos.csv'.
3. Preprocesamos los datos: eliminamos las filas con valores nulos, las filas duplicadas y convertimos la columna 'edad' a tipo entero.
4. Creamos las variables dependiente e independientes. La variable dependiente es la columna 'clase' y las variables independientes son las demás columnas.
5. Dividimos los datos en train y test. El 80% de los datos se utilizarán para entrenar el modelo y el 20% restante para evaluar el modelo.
6. Entrenamos el modelo. En este caso, utilizamos un modelo de regresión logística.
7. Evaluamos el modelo. Calculamos la precisión, el recall y el F1-score.
8. Graficamos los resultados. Graficamos los datos reales y los datos predichos por el modelo en función de la edad.