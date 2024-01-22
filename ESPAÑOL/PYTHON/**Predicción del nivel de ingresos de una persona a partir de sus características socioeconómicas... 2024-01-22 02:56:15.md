```python
# Importamos las bibliotecas necesarias
import numpy as np
import matplotlib.pyplot as plt
import pandas as pd
import scipy.stats as stats
import sklearn.linear_model as linear_model
import sklearn.model_selection as model_selection
import sklearn.metrics as metrics

# Cargamos el dataset
dataset = pd.read_csv('dataset.csv')

# Preprocesamos el dataset
dataset['edad'] = dataset['edad'].astype('int')
dataset['sexo'] = dataset['sexo'].astype('category')
dataset['educacion'] = dataset['educacion'].astype('category')
dataset['ingresos'] = dataset['ingresos'].astype('int')

# Creamos variables dummies para las variables categóricas
dataset = pd.get_dummies(dataset, columns=['sexo', 'educacion'])

# Separamos el dataset en conjunto de entrenamiento y conjunto de prueba
X_train, X_test, y_train, y_test = model_selection.train_test_split(dataset.drop('ingreso', axis=1), dataset['ingreso'], test_size=0.2, random_state=42)

# Creamos el modelo de regresión lineal
modelo = linear_model.LinearRegression()

# Entrenamos el modelo
modelo.fit(X_train, y_train)

# Evaluamos el modelo
y_pred = modelo.predict(X_test)
print('El error cuadrático medio del modelo es:', metrics.mean_squared_error(y_test, y_pred))
print('El coeficiente de determinación del modelo es:', metrics.r2_score(y_test, y_pred))

# Creamos una gráfica para visualizar el modelo
plt.scatter(y_test, y_pred)
plt.plot(y_test, y_test, color='red')
plt.xlabel('Ingreso real')
plt.ylabel('Ingreso predicho')
plt.title('Regresión lineal')
plt.show()

# Creamos una gráfica para visualizar los coeficientes del modelo
plt.bar(X_train.columns, modelo.coef_)
plt.xlabel('Variable')
plt.ylabel('Coeficiente')
plt.title('Coeficientes del modelo')
plt.show()

# Creamos una gráfica para visualizar la distribución de los residuos del modelo
plt.hist(y_test - y_pred)
plt.xlabel('Residuo')
plt.ylabel('Frecuencia')
plt.title('Distribución de los residuos')
plt.show()

# Creamos una tabla para visualizar los resultados de las pruebas estadísticas del modelo
print('Prueba de significancia de los coeficientes del modelo:')
print(pd.DataFrame(zip(X_train.columns, modelo.coef_, modelo.pvalues), columns=['Variable', 'Coeficiente', 'p-valor']))

# Creamos una tabla para visualizar los resultados de la prueba de normalidad de los residuos del modelo
print('Prueba de normalidad de los residuos del modelo:')
print(stats.normaltest(y_test - y_pred))

# Creamos una tabla para visualizar los resultados de la prueba de autocorrelación de los residuos del modelo
print('Prueba de autocorrelación de los residuos del modelo:')
print(stats.durbin_watson(y_test, y_pred))
```

Explicación del código:

1. Importamos las bibliotecas necesarias.
2. Cargamos el dataset.
3. Preprocesamos el dataset.
4. Creamos variables dummies para las variables categóricas.
5. Separamos el dataset en conjunto de entrenamiento y conjunto de prueba.
6. Creamos el modelo de regresión lineal.
7. Entrenamos el modelo.
8. Evaluamos el modelo.
9. Creamos una gráfica para visualizar el modelo.
10. Creamos una gráfica para visualizar los coeficientes del modelo.
11. Creamos una gráfica para visualizar la distribución de los residuos del modelo.
12. Creamos una tabla para visualizar los resultados de las pruebas estadísticas del modelo.
13. Creamos una tabla para visualizar los resultados de la prueba de normalidad de los residuos del modelo.
14. Creamos una tabla para visualizar los resultados de la prueba de autocorrelación de los residuos del modelo.