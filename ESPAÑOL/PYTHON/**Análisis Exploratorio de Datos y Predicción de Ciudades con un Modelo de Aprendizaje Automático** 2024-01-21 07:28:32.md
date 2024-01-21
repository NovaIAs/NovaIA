```python
# Importamos las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Creamos un conjunto de datos de muestra
datos = pd.DataFrame({
    "Nombre": ["Juan", "María", "Pedro", "Ana", "José"],
    "Edad": [20, 25, 30, 35, 40],
    "Sexo": ["Masculino", "Femenino", "Masculino", "Femenino", "Masculino"],
    "Ciudad": ["Madrid", "Barcelona", "Valencia", "Sevilla", "Málaga"]
})

# Realizamos un análisis exploratorio de los datos
print(datos.head())
print(datos.info())
print(datos.describe())

# Creamos una visualización de los datos
sns.scatterplot(x="Edad", y="Sexo", data=datos)
plt.show()

# Creamos un modelo de aprendizaje automático
modelo = sklearn.linear_model.LinearRegression()
modelo.fit(datos[["Edad", "Sexo"]], datos["Ciudad"])

# Realizamos predicciones con el modelo
predicciones = modelo.predict(datos[["Edad", "Sexo"]])
print(predicciones)

# Evaluamos el modelo
print(sklearn.metrics.mean_squared_error(datos["Ciudad"], predicciones))

# Guardamos el modelo
sklearn.joblib.dump(modelo, "modelo.pkl")

# Cargamos el modelo
modelo_cargado = sklearn.joblib.load("modelo.pkl")

# Realizamos predicciones con el modelo cargado
predicciones_cargadas = modelo_cargado.predict(datos[["Edad", "Sexo"]])
print(predicciones_cargadas)
```

Explicación:

1. Importamos las librerías necesarias.
2. Creamos un conjunto de datos de muestra.
3. Realizamos un análisis exploratorio de los datos.
4. Creamos una visualización de los datos.
5. Creamos un modelo de aprendizaje automático.
6. Realizamos predicciones con el modelo.
7. Evaluamos el modelo.
8. Guardamos el modelo.
9. Cargamos el modelo.
10. Realizamos predicciones con el modelo cargado.