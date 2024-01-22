```python
# Importación de bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Configuración de la visualización de datos
sns.set_style('whitegrid')
plt.rcParams['font.family'] = 'Arial'
plt.rcParams['font.size'] = 12

# Carga del dataset
data = pd.read_csv('data.csv')

# Exploración y limpieza de datos
# Se eliminan las filas con valores faltantes
data = data.dropna()

# Se convierten las variables categóricas a numéricas mediante codificación "one-hot"
data = pd.get_dummies(data, columns=['variable_categorica_1', 'variable_categorica_2'])

# Se normalizan las variables numéricas
data = (data - data.min()) / (data.max() - data.min())

# División del dataset en conjunto de entrenamiento y conjunto de prueba
X_train, X_test, y_train, y_test = train_test_split(data.drop('variable_objetivo', axis=1), data['variable_objetivo'], test_size=0.2, random_state=42)

# Entrenamiento del modelo
model = LinearRegression()
model.fit(X_train, y_train)

# Evaluación del modelo
score = model.score(X_test, y_test)
print('El score del modelo es:', score)

# Predicción de valores nuevos
predicciones = model.predict(X_test)
plt.scatter(y_test, predicciones)
plt.xlabel('Valores reales')
plt.ylabel('Valores predichos')
plt.show()

# Guardado del modelo
joblib.dump(model, 'modelo.pkl')

```

Este código realiza un análisis de datos complejo utilizando bibliotecas como NumPy, Pandas, Matplotlib y Seaborn. Lleva a cabo la limpieza de datos, la codificación de variables categóricas, la normalización de variables numéricas y la división del dataset en conjuntos de entrenamiento y prueba. Luego, entrena un modelo de regresión lineal y lo evalúa utilizando el conjunto de prueba. Finalmente, predice los valores de la variable objetivo para nuevos datos y guarda el modelo entrenado para su uso futuro.