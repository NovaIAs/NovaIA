```python
# Importamos las librerías necesarias.
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos el conjunto de datos.
datos = pd.read_csv('datos.csv')

# Exploramos los datos.
print(datos.head())
print(datos.describe())
print(sns.heatmap(datos.corr(), annot=True))

# Creamos un modelo de regresión lineal.
modelo = LinearRegression()
modelo.fit(datos[['característica_1', 'característica_2']], datos['objetivo'])

# Evaluamos el modelo.
print(modelo.score(datos[['característica_1', 'característica_2']], datos['objetivo']))

# Hacemos predicciones.
predicciones = modelo.predict(datos[['característica_1', 'característica_2']])

# Visualizamos los resultados.
plt.scatter(datos['característica_1'], datos['objetivo'])
plt.plot(datos['característica_1'], predicciones, color='rojo')
plt.show()
```

**Explicación:**

Este código realiza un análisis exploratorio de datos y crea un modelo de regresión lineal para predecir una variable objetivo a partir de dos variables características.

1. **Importación de librerías:** Se importan las librerías necesarias para el análisis de datos y la creación del modelo.

2. **Carga del conjunto de datos:** Se carga el conjunto de datos desde un archivo CSV.

3. **Exploración de datos:** Se muestran las primeras filas del conjunto de datos, se calculan estadísticas descriptivas y se crea una matriz de correlación para explorar las relaciones entre las variables.

4. **Creación del modelo de regresión lineal:** Se crea un modelo de regresión lineal utilizando las dos variables características como variables independientes y la variable objetivo como variable dependiente.

5. **Evaluación del modelo:** Se evalúa el modelo calculando el coeficiente de determinación R^2, que indica la proporción de varianza de la variable objetivo que es explicada por el modelo.

6. **Predicciones:** Se hacen predicciones de la variable objetivo utilizando el modelo.

7. **Visualización de los resultados:** Se visualizan los datos y las predicciones en un gráfico de dispersión para mostrar la relación entre las variables características y la variable objetivo.