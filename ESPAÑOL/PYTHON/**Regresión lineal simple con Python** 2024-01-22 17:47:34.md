```python
# Importar las librerías necesarias.
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargar el conjunto de datos.
datos = pd.read_csv('datos.csv')

# Preprocesamiento de los datos.
datos.dropna(inplace=True)
datos['caracteristica_1'] = datos['caracteristica_1'].astype(float)
datos['caracteristica_2'] = datos['caracteristica_2'].astype(float)

# Dividir el conjunto de datos en entrenamiento y prueba.
X_train, X_test, y_train, y_test = train_test_split(datos[['caracteristica_1', 'caracteristica_2']], datos['etiqueta'], test_size=0.2, random_state=42)

# Ajustar el modelo de regresión lineal.
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluar el modelo de regresión lineal.
y_pred = modelo.predict(X_test)
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)
print('MSE:', mse)
print('R2:', r2)

# Visualizar los resultados.
plt.scatter(X_test['caracteristica_1'], y_test, color='blue')
plt.plot(X_test['caracteristica_1'], y_pred, color='red')
plt.title('Regresión lineal simple')
plt.xlabel('caracteristica_1')
plt.ylabel('etiqueta')
plt.show()

# Guardar el modelo.
import pickle
with open('modelo.pkl', 'wb') as file:
    pickle.dump(modelo, file)
```

Explicación del código:

1. Importamos las librerías necesarias.
2. Cargamos el conjunto de datos desde un archivo CSV.
3. Preprocesamos los datos eliminando los valores faltantes y convirtiendo las características a tipo flotante.
4. Dividimos el conjunto de datos en entrenamiento y prueba.
5. Ajustamos el modelo de regresión lineal a los datos de entrenamiento.
6. Evaluamos el modelo de regresión lineal utilizando la métrica de error cuadrático medio (MSE) y el coeficiente de determinación (R2).
7. Visualizamos los resultados de la regresión lineal.
8. Guardamos el modelo en un archivo para poder utilizarlo posteriormente.