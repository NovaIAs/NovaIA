```python
# Importamos las bibliotecas necesarias
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns

# Creamos un conjunto de datos aleatorio
datos = np.random.randn(100, 5)

# Convertimos el conjunto de datos en un DataFrame de Pandas
df = pd.DataFrame(datos, columns=["Columna 1", "Columna 2", "Columna 3", "Columna 4", "Columna 5"])

# Imprimimos las primeras cinco filas del DataFrame
print(df.head())

# Creamos un gráfico de dispersión de los datos
plt.scatter(df["Columna 1"], df["Columna 2"])
plt.xlabel("Columna 1")
plt.ylabel("Columna 2")
plt.show()

# Creamos un histograma de los datos de la Columna 3
sns.histplot(df["Columna 3"])
plt.xlabel("Columna 3")
plt.ylabel("Frecuencia")
plt.show()

# Creamos un gráfico de barras de los datos de la Columna 4
plt.bar(df["Columna 4"], df["Columna 5"])
plt.xlabel("Columna 4")
plt.ylabel("Columna 5")
plt.show()

# Creamos un gráfico de líneas de los datos de la Columna 5
plt.plot(df["Columna 5"])
plt.xlabel("Índice")
plt.ylabel("Columna 5")
plt.show()

# Creamos una matriz de correlación de los datos
corr = df.corr()

# Imprimimos la matriz de correlación
print(corr)

# Creamos un mapa de calor de la matriz de correlación
sns.heatmap(corr, annot=True)
plt.show()

# Creamos un modelo de regresión lineal para predecir los datos de la Columna 5 a partir de los datos de las otras columnas
modelo = sklearn.linear_model.LinearRegression()
modelo.fit(df.drop("Columna 5", axis=1), df["Columna 5"])

# Imprimimos el coeficiente de determinación del modelo
print(modelo.score(df.drop("Columna 5", axis=1), df["Columna 5"]))

# Creamos una predicción de los datos de la Columna 5
prediccion = modelo.predict(df.drop("Columna 5", axis=1))

# Imprimimos la predicción
print(prediccion)

# Creamos un gráfico de dispersión de los datos reales y los datos predichos
plt.scatter(df["Columna 5"], prediccion)
plt.xlabel("Datos reales")
plt.ylabel("Datos predichos")
plt.show()
```

**Explicación del código:**

1. Importamos las bibliotecas necesarias.
2. Creamos un conjunto de datos aleatorio.
3. Convertimos el conjunto de datos en un DataFrame de Pandas.
4. Imprimimos las primeras cinco filas del DataFrame.
5. Creamos un gráfico de dispersión de los datos.
6. Creamos un histograma de los datos de la Columna 3.
7. Creamos un gráfico de barras de los datos de la Columna 4.
8. Creamos un gráfico de líneas de los datos de la Columna 5.
9. Creamos una matriz de correlación de los datos.
10. Imprimimos la matriz de correlación.
11. Creamos un mapa de calor de la matriz de correlación.
12. Creamos un modelo de regresión lineal para predecir los datos de la Columna 5 a partir de los datos de las otras columnas.
13. Imprimimos el coeficiente de determinación del modelo.
14. Creamos una predicción de los datos de la Columna 5.
15. Imprimimos la predicción.
16. Creamos un gráfico de dispersión de los datos reales y los datos predichos.