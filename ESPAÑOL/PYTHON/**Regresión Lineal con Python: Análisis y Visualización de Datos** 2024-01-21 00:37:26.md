```python
# Importamos las bibliotecas necesarias
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos el dataset
df = pd.read_csv('data.csv')

# Creamos una lista con las columnas numéricas
columnas_numericas = [col for col in df.columns if df[col].dtype == 'int64' or df[col].dtype == 'float64']

# Creamos una lista con las columnas categóricas
columnas_categoricas = [col for col in df.columns if col not in columnas_numericas]

# Convertimos las columnas categóricas a variables dummy
df = pd.get_dummies(df, columns=columnas_categoricas)

# Dividimos el dataset en conjunto de entrenamiento y conjunto de prueba
X_train, X_test, y_train, y_test = train_test_split(df[columnas_numericas], df['target'], test_size=0.2, random_state=42)

# Creamos un modelo de regresión lineal
modelo = LinearRegression()

# Entrenamos el modelo
modelo.fit(X_train, y_train)

# Evaluamos el modelo
print('R2 score:', modelo.score(X_test, y_test))

# Hacemos predicciones sobre el conjunto de prueba
predicciones = modelo.predict(X_test)

# Dibujamos un diagrama de dispersión con las predicciones y los valores reales
plt.scatter(y_test, predicciones)
plt.xlabel('Valores reales')
plt.ylabel('Predicciones')
plt.title('Diagrama de dispersión de las predicciones')
plt.show()

# Creamos un gráfico de residuos
plt.plot(y_test - predicciones)
plt.xlabel('Índice de la observación')
plt.ylabel('Residuo')
plt.title('Gráfico de residuos')
plt.show()
```

Explicación del código:

1. Importamos las bibliotecas necesarias para el análisis de datos y la visualización.
2. Cargamos el dataset en un dataframe de Pandas.
3. Creamos una lista con las columnas numéricas y otra lista con las columnas categóricas.
4. Convertimos las columnas categóricas a variables dummy utilizando la función `get_dummies()` de Pandas.
5. Dividimos el dataset en conjunto de entrenamiento y conjunto de prueba utilizando la función `train_test_split()` de scikit-learn.
6. Creamos un modelo de regresión lineal utilizando la clase `LinearRegression()` de scikit-learn.
7. Entrenamos el modelo con el conjunto de entrenamiento.
8. Evaluamos el modelo con el conjunto de prueba y calculamos el R2 score.
9. Hacemos predicciones sobre el conjunto de prueba.
10. Dibujamos un diagrama de dispersión con las predicciones y los valores reales.
11. Creamos un gráfico de residuos para visualizar la diferencia entre las predicciones y los valores reales.