```python
# Importación de bibliotecas
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Carga de datos
datos = pd.read_csv('datos.csv')

# Preprocesamiento de datos
datos.dropna(inplace=True)
datos.fillna(datos.mean(), inplace=True)

# Creación de variables dependientes e independientes
y = datos['variable_dependiente']
X = datos.drop('variable_dependiente', axis=1)

# División de datos en train y test
X_train, X_test, y_train, y_test = train_test_split(X, y, test_size=0.2, random_state=42)

# Entrenamiento del modelo
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluación del modelo
score = modelo.score(X_test, y_test)
print('El score del modelo es:', score)

# Predicción de valores
predicciones = modelo.predict(X_test)

# Gráfica de los datos reales y los predichos
plt.scatter(y_test, predicciones)
plt.xlabel('Valores reales')
plt.ylabel('Valores predichos')
plt.title('Comparación de datos reales y predichos')
plt.show()

# Análisis de la importancia de las variables
importances = modelo.coef_
feature_names = X_test.columns
for i, importance in enumerate(importances):
    print(f'{feature_names[i]}: {importance}')

# Guardado del modelo
modelo.save('modelo.pkl')
```

Explicación del código:

1. **Importación de bibliotecas:** Se importan las bibliotecas necesarias para el análisis de datos y el modelado estadístico.

2. **Carga de datos:** Se cargan los datos del archivo `datos.csv` utilizando la función `pd.read_csv()`.

3. **Preprocesamiento de datos:** Se eliminan las filas que contienen valores faltantes y se rellenan los valores faltantes con la media de cada columna.

4. **Creación de variables dependientes e independientes:** Se divide el conjunto de datos en variables dependientes e independientes. La variable dependiente es la que queremos predecir (en este caso, `variable_dependiente`), y las variables independientes son las que utilizamos para hacer la predicción (en este caso, todas las demás columnas del conjunto de datos).

5. **División de datos en train y test:** Se divide el conjunto de datos en dos partes: un conjunto de entrenamiento y un conjunto de prueba. El conjunto de entrenamiento se utiliza para entrenar el modelo, y el conjunto de prueba se utiliza para evaluar el rendimiento del modelo.

6. **Entrenamiento del modelo:** Se entrena el modelo lineal utilizando la función `LinearRegression()`. El modelo se entrena utilizando el conjunto de entrenamiento.

7. **Evaluación del modelo:** Se evalúa el rendimiento del modelo utilizando la función `score()`. El score se calcula como la correlación entre los valores reales y los valores predichos.

8. **Predicción de valores:** Se utilizan los datos del conjunto de prueba para hacer predicciones utilizando el modelo entrenado.

9. **Gráfica de los datos reales y los predichos:** Se crea una gráfica que muestra los valores reales y los valores predichos. Esta gráfica se puede utilizar para visualizar el rendimiento del modelo.

10. **Análisis de la importancia de las variables:** Se calcula la importancia de cada variable independiente en la predicción de la variable dependiente. La importancia de una variable se calcula como el coeficiente de la variable en el modelo lineal.

11. **Guardado del modelo:** Se guarda el modelo entrenado en un archivo utilizando la función `save()`. Esto permite utilizar el modelo para hacer predicciones en el futuro.