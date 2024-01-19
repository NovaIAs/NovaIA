```python
# Importar las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargar el conjunto de datos
datos = pd.read_csv('datos.csv')

# Preprocesar los datos
datos = datos.dropna()
datos = datos.astype(float)

# Crear variables dependientes e independientes
X = datos.drop('variable_dependiente', axis=1)
y = datos['variable_dependiente']

# Dividir los datos en conjuntos de entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrenar el modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluar el modelo de regresión lineal
y_pred = modelo.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print('MSE:', mse)
print('R2:', r2)

# Crear un gráfico de dispersión de los datos reales y los datos predichos
plt.scatter(y_test, y_pred)
plt.xlabel('Datos reales')
plt.ylabel('Datos predichos')
plt.title('Gráfico de dispersión de los datos reales y los datos predichos')
plt.show()

# Crear un gráfico de residuos del modelo de regresión lineal
plt.scatter(y_test, y_pred - y_test)
plt.xlabel('Datos reales')
plt.ylabel('Residuos')
plt.title('Gráfico de residuos del modelo de regresión lineal')
plt.show()
```

Explicación:

1. Primero, importamos las bibliotecas necesarias para el análisis de datos y la creación de modelos de aprendizaje automático.

2. A continuación, cargamos el conjunto de datos en un DataFrame de Pandas.

3. Una vez cargado el conjunto de datos, lo preprocesamos para asegurarnos de que no hay valores faltantes y de que todos los datos son de tipo numérico.

4. Luego, creamos las variables dependientes e independientes. La variable dependiente es la variable que queremos predecir, y las variables independientes son las variables que utilizaremos para predecirla.

5. A continuación, dividimos los datos en conjuntos de entrenamiento y prueba. El conjunto de entrenamiento se utilizará para entrenar el modelo, y el conjunto de prueba se utilizará para evaluar el rendimiento del modelo.

6. En este paso, entrenamos el modelo de regresión lineal utilizando el conjunto de entrenamiento. La regresión lineal es un algoritmo de aprendizaje automático que se utiliza para predecir el valor de una variable continua en función de una o más variables independientes.

7. Después de entrenar el modelo, lo evaluamos utilizando el conjunto de prueba. Calculamos el error cuadrático medio (MSE) y el coeficiente de determinación (R2) para medir el rendimiento del modelo.

8. Por último, creamos dos gráficos para visualizar los resultados del modelo. El primer gráfico es un gráfico de dispersión que muestra los datos reales y los datos predichos por el modelo. El segundo gráfico es un gráfico de residuos que muestra los residuos del modelo, que es la diferencia entre los datos reales y los datos predichos.