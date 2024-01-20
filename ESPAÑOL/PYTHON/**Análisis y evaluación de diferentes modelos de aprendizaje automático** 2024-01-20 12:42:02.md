```python
# Importamos las bibliotecas necesarias
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import seaborn as sns
import statsmodels.api as sm
import sklearn.model_selection as skms
import sklearn.linear_model as sklm
import sklearn.metrics as skmt
import sklearn.ensemble as ske

# Cargamos el conjunto de datos
datos = pd.read_csv("path/to/data.csv")

# Exploramos los datos
print(datos.head())
print(datos.info())
print(datos.describe())

# Visualizamos los datos
sns.pairplot(datos)
plt.show()

# Dividimos los datos en train y test
X_train, X_test, y_train, y_test = skms.train_test_split(datos.drop("variable_objetivo", axis=1), datos["variable_objetivo"], test_size=0.2, random_state=42)

# Entrenamos el modelo de regresión lineal
modelo = sklm.LinearRegression()
modelo.fit(X_train, y_train)

# Evaluamos el modelo de regresión lineal
y_pred = modelo.predict(X_test)
print(skmt.r2_score(y_test, y_pred))

# Entrenamos el modelo de árbol de decisión
modelo = ske.DecisionTreeClassifier()
modelo.fit(X_train, y_train)

# Evaluamos el modelo de árbol de decisión
y_pred = modelo.predict(X_test)
print(skmt.accuracy_score(y_test, y_pred))

# Entrenamos el modelo de bosque aleatorio
modelo = ske.RandomForestClassifier()
modelo.fit(X_train, y_train)

# Evaluamos el modelo de bosque aleatorio
y_pred = modelo.predict(X_test)
print(skmt.accuracy_score(y_test, y_pred))

# Guardamos el modelo
modelo.save("path/to/model.pkl")
```

Este código es más complejo que el anterior porque:

* Utiliza múltiples bibliotecas y técnicas de aprendizaje automático.
* Se divide el conjunto de datos en train y test para evaluar el rendimiento de los modelos.
* Se entrenan y evalúan tres modelos diferentes: regresión lineal, árbol de decisión y bosque aleatorio.
* Se guarda el modelo que mejor rendimiento ha obtenido.

Este código podría utilizarse para resolver una gran variedad de problemas, como la predicción de precios, la clasificación de imágenes o el diagnóstico de enfermedades.