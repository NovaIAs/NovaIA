```python
# Importar las bibliotecas necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Crear un conjunto de datos de muestra
datos = pd.DataFrame({
    "nombre": ["Juan", "María", "Pedro", "Ana", "José"],
    "edad": [20, 25, 30, 35, 40],
    "sexo": ["masculino", "femenino", "masculino", "femenino", "masculino"]
})

# Mostrar el conjunto de datos
print(datos)

# Crear una matriz de correlación
matriz_correlacion = datos.corr()

# Mostrar la matriz de correlación
print(matriz_correlacion)

# Crear un diagrama de dispersión de la edad y el sexo
plt.scatter(datos["edad"], datos["sexo"])

# Mostrar el diagrama de dispersión
plt.show()

# Crear un histograma de la edad
plt.hist(datos["edad"])

# Mostrar el histograma
plt.show()

# Crear un gráfico de barras del sexo
plt.bar(datos["sexo"], datos["edad"])

# Mostrar el gráfico de barras
plt.show()

# Crear un gráfico circular del sexo
plt.pie(datos["sexo"], labels=datos["nombre"])

# Mostrar el gráfico circular
plt.show()

# Crear un modelo de regresión lineal para predecir la edad a partir del sexo
modelo = smf.OLS(datos["edad"], datos["sexo"])

# Ajustar el modelo
resultado = modelo.fit()

# Mostrar el resumen del modelo
print(resultado.summary())

# Hacer una predicción de la edad para un sexo dado
prediccion = modelo.predict(pd.DataFrame({"sexo": ["masculino"]}))

# Mostrar la predicción
print(prediccion)
```

Explicación del código:

* Se importan las bibliotecas necesarias.
* Se crea un conjunto de datos de muestra.
* Se muestra el conjunto de datos.
* Se crea una matriz de correlación.
* Se muestra la matriz de correlación.
* Se crea un diagrama de dispersión de la edad y el sexo.
* Se muestra el diagrama de dispersión.
* Se crea un histograma de la edad.
* Se muestra el histograma.
* Se crea un gráfico de barras del sexo.
* Se muestra el gráfico de barras.
* Se crea un gráfico circular del sexo.
* Se muestra el gráfico circular.
* Se crea un modelo de regresión lineal para predecir la edad a partir del sexo.
* Se ajusta el modelo.
* Se muestra el resumen del modelo.
* Se hace una predicción de la edad para un sexo dado.
* Se muestra la predicción.