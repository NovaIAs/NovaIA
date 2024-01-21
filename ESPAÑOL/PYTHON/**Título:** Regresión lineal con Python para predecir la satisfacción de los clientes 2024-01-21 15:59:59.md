```python
# Importamos las librerías necesarias.
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Leemos el conjunto de datos.
datos = pd.read_csv('datos.csv')

# Limpiamos los datos.
datos = datos.dropna()

# Creamos una lista con las variables independientes.
variables_independientes = ['edad', 'sexo', 'educación', 'ingresos']

# Creamos una lista con las variables dependientes.
variables_dependientes = ['satisfacción']

# Separamos el conjunto de datos en un conjunto de entrenamiento y un conjunto de prueba.
X_train, X_test, y_train, y_test = train_test_split(datos[variables_independientes], datos[variables_dependientes], test_size=0.2)

# Entrenamos el modelo.
modelo = LinearRegression()
modelo.fit(X_train, y_train)

# Evaluamos el modelo.
score = modelo.score(X_test, y_test)
print('El score del modelo es:', score)

# Hacemos predicciones.
y_pred = modelo.predict(X_test)

# Visualizamos los resultados.
plt.scatter(y_test, y_pred)
plt.xlabel('Valor real')
plt.ylabel('Valor predicho')
plt.show()

# Guardamos el modelo.
pickle.dump(modelo, open('modelo.pkl', 'wb'))
```

**Explicación del código:**

1. Importamos las librerías necesarias.

2. Leemos el conjunto de datos.

3. Limpiamos los datos.

4. Creamos una lista con las variables independientes y una lista con las variables dependientes.

5. Separamos el conjunto de datos en un conjunto de entrenamiento y un conjunto de prueba.

6. Entrenamos el modelo.

7. Evaluamos el modelo.

8. Hacemos predicciones.

9. Visualizamos los resultados.

10. Guardamos el modelo.