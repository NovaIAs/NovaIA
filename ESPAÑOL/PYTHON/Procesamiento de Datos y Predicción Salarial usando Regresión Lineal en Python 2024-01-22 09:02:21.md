```python
# Importamos las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos los datos
datos = pd.read_csv('datos.csv')

# Limpiamos los datos
datos = datos.dropna()
datos = datos.drop_duplicates()

# Convertimos las variables categóricas a numéricas
datos = pd.get_dummies(datos, columns=['sexo', 'estado_civil'])

# Creamos el modelo
modelo = linear_model.LinearRegression()

# Entrenamos el modelo
modelo.fit(datos[['edad', 'sexo', 'estado_civil']], datos['salario'])

# Hacemos predicciones
predicciones = modelo.predict(datos[['edad', 'sexo', 'estado_civil']])

# Evaluamos el modelo
r2 = metrics.r2_score(datos['salario'], predicciones)
print('El coeficiente de determinación R^2 es:', r2)

# Visualizamos los datos
sns.scatterplot(datos['edad'], datos['salario'], color='blue')
sns.lineplot(datos['edad'], predicciones, color='red')
plt.show()
```

Este código es un ejemplo de cómo crear y evaluar un modelo de regresión lineal utilizando la librería `scikit-learn` en Python. El código se puede dividir en los siguientes pasos:

1. **Importación de las librerías necesarias:** Se importan las librerías `numpy`, `pandas`, `matplotlib.pyplot` y `seaborn`.
2. **Carga de los datos:** Se cargan los datos desde un archivo CSV utilizando la función `pd.read_csv()`.
3. **Limpieza de los datos:** Se eliminan las filas con valores faltantes y las filas duplicadas.
4. **Conversión de las variables categóricas a numéricas:** Se utilizan las funciones `pd.get_dummies()` para convertir las variables categóricas en variables numéricas.
5. **Creación del modelo:** Se crea un modelo de regresión lineal utilizando la función `linear_model.LinearRegression()`.
6. **Entrenamiento del modelo:** Se entrena el modelo utilizando el método `fit()` del modelo.
7. **Predicción de los valores:** Se hacen predicciones de los valores de la variable dependiente utilizando el método `predict()` del modelo.
8. **Evaluación del modelo:** Se evalúa el modelo utilizando el coeficiente de determinación R^2.
9. **Visualización de los datos:** Se visualizan los datos utilizando la función `sns.scatterplot()` y la función `sns.lineplot()`.

Este código es un ejemplo de un código complejo en Python que difícilmente se repetirá nuevamente. El código cubre una amplia gama de temas, incluyendo:

* Carga y limpieza de datos
* Conversión de variables categóricas a numéricas
* Creación, entrenamiento y evaluación de un modelo de regresión lineal
* Visualización de datos

Este código es un buen ejemplo de cómo utilizar Python para realizar un análisis de datos completo.