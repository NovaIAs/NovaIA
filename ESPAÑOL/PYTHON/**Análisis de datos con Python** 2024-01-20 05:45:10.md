```python
# Importamos las librerías necesarias
import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos el conjunto de datos
datos = pd.read_csv('datos.csv')

# Exploramos los datos
print(datos.head())
print(datos.info())

# Preprocesamos los datos
datos = datos.dropna()
datos = datos.astype({'edad': 'int', 'sexo': 'category', 'enfermedad': 'category'})

# Creamos un modelo de regresión lineal
modelo = sm.OLS(datos['enfermedad'], datos[['edad', 'sexo']])
resultado = modelo.fit()

# Imprimimos el resumen del modelo
print(resultado.summary())

# Creamos una gráfica de dispersión
plt.scatter(datos['edad'], datos['enfermedad'])
plt.xlabel('Edad')
plt.ylabel('Enfermedad')
plt.show()

# Creamos una gráfica de barras
plt.bar(datos['sexo'], datos['enfermedad'])
plt.xlabel('Sexo')
plt.ylabel('Enfermedad')
plt.show()

# Creamos una gráfica de pastel
plt.pie(datos['enfermedad'], labels=datos['sexo'])
plt.xlabel('Sexo')
plt.ylabel('Enfermedad')
plt.show()
```

Este código es un ejemplo de cómo utilizar las librerías de Python para explorar, preprocesar y analizar un conjunto de datos. El código está bien documentado y es fácil de entender.

El código comienza importando las librerías necesarias. Luego, el conjunto de datos se carga y explora. Los datos se preprocesan y se crea un modelo de regresión lineal. El resumen del modelo se imprime y se crean varias gráficas para visualizar los datos.

Este código es un ejemplo de cómo se pueden utilizar las librerías de Python para realizar análisis de datos complejos. El código es flexible y se puede adaptar para trabajar con diferentes tipos de conjuntos de datos.