```python
# Importamos las librerías necesarias.
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Creamos un dataframe con datos aleatorios.
df = pd.DataFrame(
    np.random.rand(100, 5),
    columns=["Col1", "Col2", "Col3", "Col4", "Col5"]
)

# Visualizamos las correlaciones entre las columnas.
corr_matrix = df.corr()
sns.heatmap(corr_matrix, annot=True)
plt.show()

# Entrenamos un modelo de regresión lineal para predecir la columna "Col5".
X = df.drop("Col5", axis=1)  # Variables independientes.
y = df["Col5"]  # Variable dependiente.
model = LinearRegression()
model.fit(X, y)

# Evaluamos el modelo.
print("R2:", model.score(X, y))
print("Coeficientes:", model.coef_)
print("Intercepto:", model.intercept_)

# Hacemos predicciones.
y_pred = model.predict(X)

# Visualizamos las predicciones frente a los valores reales.
plt.scatter(y, y_pred)
plt.xlabel("Valores reales")
plt.ylabel("Predicciones")
plt.show()

# Aplicamos el modelo a un nuevo dataframe.
df_new = pd.DataFrame(
    np.random.rand(10, 5),
    columns=["Col1", "Col2", "Col3", "Col4", "Col5"]
)
y_pred_new = model.predict(df_new.drop("Col5", axis=1))

# Imprimimos las predicciones.
print("Predicciones para el nuevo dataframe:")
print(y_pred_new)
```

Explicación del código:

1. Importamos las librerías necesarias.
2. Creamos un dataframe con datos aleatorios.
3. Visualizamos las correlaciones entre las columnas.
4. Entrenamos un modelo de regresión lineal para predecir la columna "Col5".
5. Evaluamos el modelo.
6. Hacemos predicciones.
7. Visualizamos las predicciones frente a los valores reales.
8. Aplicamos el modelo a un nuevo dataframe.
9. Imprimimos las predicciones.