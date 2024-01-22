```python
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargar el dataset
datos = pd.read_csv('datos.csv')

# Preprocesamiento de datos:
# Eliminar filas con valores faltantes
datos = datos.dropna()

# Convertir variables categóricas a numéricas
datos['sexo'] = pd.get_dummies(datos['sexo'], drop_first=True)
datos['estado_civil'] = pd.get_dummies(datos['estado_civil'], drop_first=True)

# Dividir el dataset en entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(datos.drop('salario', axis=1), datos['salario'], test_size=0.2, random_state=42)

# Entrenar el modelo de regresión lineal
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluar el modelo en el conjunto de prueba
y_pred = modelo.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Imprimir los resultados
print(f'MSE: {mse}')
print(f'R2: {r2}')

# Generar gráfico de dispersión de los valores reales y predichos
plt.scatter(y_test, y_pred)
plt.xlabel('Valores reales')
plt.ylabel('Valores predichos')
plt.show()

# Generar gráfico de barras con las importancias de las variables
coefs = pd.DataFrame({'variable': X_train.columns, 'importancia': modelo.coef_})
coefs = coefs.sort_values('importancia', ascending=False)
plt.bar(coefs['variable'], coefs['importancia'])
plt.xlabel('Variable')
plt.ylabel('Importancia')
plt.show()
```

Explicación:

1. Importamos las bibliotecas necesarias.
2. Cargamos el dataset en un DataFrame.
3. Realizamos preprocesamiento de datos: eliminamos filas con valores faltantes y convertimos variables categóricas a numéricas.
4. Dividimos el dataset en entrenamiento y prueba.
5. Entrenamos el modelo de regresión lineal.
6. Evaluamos el modelo en el conjunto de prueba.
7. Imprimimos los resultados de la evaluación.
8. Generamos un gráfico de dispersión de los valores reales y predichos.
9. Generamos un gráfico de barras con las importancias de las variables.