```python
# Importamos las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
from sklearn.model_selection import train_test_split
from sklearn.linear_model import LinearRegression
from sklearn.metrics import mean_squared_error, r2_score

# Cargamos el conjunto de datos
data = pd.read_csv("datos.csv")

# Dividimos el conjunto de datos en entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(data.drop("variable_objetivo", axis=1), data["variable_objetivo"], test_size=0.2, random_state=42)

# Creamos el modelo de regresión lineal
model = LinearRegression()

# Entrenamos el modelo con los datos de entrenamiento
model.fit(X_train, y_train)

# Hacemos predicciones con los datos de prueba
y_pred = model.predict(X_test)

# Evaluamos el modelo
mse = mean_squared_error(y_test, y_pred)
r2 = r2_score(y_test, y_pred)

# Imprimimos los resultados
print("Error cuadrático medio:", mse)
print("Coeficiente de determinación:", r2)

# Creamos una gráfica con los datos reales y los datos predichos
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color="red")
plt.xlabel("Variable objetivo real")
plt.ylabel("Variable objetivo predicha")
plt.title("Regresión lineal")
plt.show()
```

Explicación:

* Importamos las bibliotecas necesarias.
* Cargamos el conjunto de datos.
* Dividimos el conjunto de datos en entrenamiento y prueba.
* Creamos el modelo de regresión lineal.
* Entrenamos el modelo con los datos de entrenamiento.
* Hacemos predicciones con los datos de prueba.
* Evaluamos el modelo.
* Imprimimos los resultados.
* Creamos una gráfica con los datos reales y los datos predichos.