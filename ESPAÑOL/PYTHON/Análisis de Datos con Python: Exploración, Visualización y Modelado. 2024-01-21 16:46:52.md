```python
# Importación de librerías
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Creación de un DataFrame
df = pd.DataFrame({
    "Nombre": ["Juan", "María", "Pedro", "Ana", "José"],
    "Edad": [20, 25, 30, 35, 40],
    "Sexo": ["Masculino", "Femenino", "Masculino", "Femenino", "Masculino"],
    "Ciudad": ["Madrid", "Barcelona", "Valencia", "Sevilla", "Bilbao"]
})

# Mostrar el DataFrame
print(df)

# Gráfica de barras de la edad por sexo
plt.bar(df["Sexo"], df["Edad"])
plt.xlabel("Sexo")
plt.ylabel("Edad")
plt.title("Edad por Sexo")
plt.show()

# Gráfica de dispersión de la edad por ciudad
plt.scatter(df["Ciudad"], df["Edad"])
plt.xlabel("Ciudad")
plt.ylabel("Edad")
plt.title("Edad por Ciudad")
plt.show()

# Cálculo de la correlación entre la edad y el sexo
correlacion = df["Edad"].corr(df["Sexo"])
print("Correlación entre la edad y el sexo:", correlacion)

# Creación de un modelo de regresión lineal para predecir la edad a partir del sexo
modelo = sm.OLS(df["Edad"], df["Sexo"])
resultado = modelo.fit()
print("Resumen del modelo:", resultado.summary())

# Predicción de la edad para un sexo determinado
sexo = "Masculino"
edad_predicha = resultado.predict(pd.DataFrame({"Sexo": [sexo]}))
print("Edad predicha para un sexo determinado:", edad_predicha)
```

Este código es un ejemplo de análisis de datos utilizando las librerías NumPy, Pandas, Matplotlib y Seaborn. El código realiza las siguientes tareas:

* Carga los datos en un DataFrame de Pandas.
* Muestra el DataFrame.
* Crea una gráfica de barras de la edad por sexo.
* Crea una gráfica de dispersión de la edad por ciudad.
* Calcula la correlación entre la edad y el sexo.
* Crea un modelo de regresión lineal para predecir la edad a partir del sexo.
* Predice la edad para un sexo determinado.

El código está bien documentado y es fácil de entender.