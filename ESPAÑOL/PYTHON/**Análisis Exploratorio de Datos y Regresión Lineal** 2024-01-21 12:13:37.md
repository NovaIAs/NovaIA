```python
# Importar las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Crear un DataFrame con datos aleatorios
df = pd.DataFrame(np.random.randn(100, 5), columns=["A", "B", "C", "D", "E"])

# Mostrar las primeras 5 filas del DataFrame
print(df.head())

# Crear una gráfica de dispersión de las columnas A y B
sns.scatterplot(x="A", y="B", data=df)
plt.show()

# Calcular la correlación entre las columnas A y B
corr = df["A"].corr(df["B"])
print("Correlación entre A y B:", corr)

# Crear un modelo de regresión lineal para predecir la columna B a partir de la columna A
model = LinearRegression()
model.fit(df[["A"]], df["B"])

# Hacer predicciones de la columna B utilizando el modelo
predictions = model.predict(df[["A"]])

# Calcular el error cuadrático medio (MSE) del modelo
mse = mean_squared_error(df["B"], predictions)
print("Error cuadrático medio:", mse)

# Crear una gráfica de los datos reales y los datos predichos
plt.scatter(df["A"], df["B"])
plt.plot(df["A"], predictions, color="red")
plt.show()
```

Este código realiza un análisis de datos exploratorio de un DataFrame con datos aleatorios. Primero, crea un DataFrame con datos aleatorios y lo muestra en pantalla. Luego, crea una gráfica de dispersión de las columnas A y B y calcula la correlación entre estas dos columnas. A continuación, crea un modelo de regresión lineal para predecir la columna B a partir de la columna A y calcula el error cuadrático medio (MSE) del modelo. Finalmente, crea una gráfica de los datos reales y los datos predichos.