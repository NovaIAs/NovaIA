```python
# Importamos las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression

# Cargamos los datos
datos = pd.read_csv('datos.csv')

# Exploramos los datos
print(datos.head())
print(datos.info())
print(datos.describe())

# Preparamos los datos
X = datos[['variable_1', 'variable_2', 'variable_3']]
y = datos['variable_objetivo']

# Dividimos los datos en train y test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrenamos el modelo
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluamos el modelo
score = modelo.score(X_test, y_test)
print('El score del modelo es:', score)

# Hacemos predicciones
predicciones = modelo.predict(X_test)

# Visualizamos los resultados
plt.scatter(y_test, predicciones)
plt.xlabel('Valores reales')
plt.ylabel('Valores predichos')
plt.title('Comparación entre valores reales y predichos')
plt.show()

# Guardamos el modelo
modelo.save('modelo.joblib')
```

Explicación del código:

1. Importamos las bibliotecas necesarias.
2. Cargamos los datos de un archivo CSV.
3. Exploramos los datos para ver su estructura y contenido.
4. Preparamos los datos dividiéndolos en variables independientes (X) y variable dependiente (y).
5. Dividimos los datos en train y test para entrenar y evaluar el modelo.
6. Entrenamos el modelo de regresión lineal usando la función `fit()`.
7. Evaluamos el modelo con la función `score()` para ver qué tan bien se ajusta a los datos de test.
8. Hacemos predicciones usando la función `predict()`.
9. Visualizamos los resultados para comparar los valores reales con los predichos.
10. Guardamos el modelo para poder usarlo en el futuro.