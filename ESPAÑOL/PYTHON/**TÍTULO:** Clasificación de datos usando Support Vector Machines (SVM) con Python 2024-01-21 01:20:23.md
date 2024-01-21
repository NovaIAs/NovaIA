```python
# Importamos las librerías necesarias
import pandas as pd
import numpy as np
import matplotlib.pyplot as plt
import seaborn as sns

# Cargamos los datos
datos = pd.read_csv('datos.csv')

# Limpiamos los datos
datos = datos.dropna()
datos = datos.drop_duplicates()

# Convertimos las variables categóricas a numéricas
datos['sexo'] = datos['sexo'].astype('category')
datos['sexo'] = datos['sexo'].cat.codes

# Creamos variables ficticias para las variables categóricas
datos = pd.get_dummies(datos, columns=['sexo'])

# Dividimos los datos en entrenamiento y prueba
X_train, X_test, y_train, y_test = train_test_split(datos.drop('clase', axis=1), datos['clase'], test_size=0.2, random_state=42)

# Normalizamos los datos
X_train = (X_train - X_train.mean()) / X_train.std()
X_test = (X_test - X_test.mean()) / X_test.std()

# Creamos el modelo
modelo = SVC()

# Entrenamos el modelo
modelo.fit(X_train, y_train)

# Evaluamos el modelo
score = modelo.score(X_test, y_test)
print('Precisión:', score)

# Hacemos una predicción
prediccion = modelo.predict(X_test)

# Calculamos la matriz de confusión
matriz_confusion = confusion_matrix(y_test, prediccion)
print('Matriz de confusión:\n', matriz_confusion)

# Visualizamos la matriz de confusión
sns.heatmap(matriz_confusion, annot=True, fmt='d')
plt.show()

# Calculamos la curva ROC
fpr, tpr, umbrales = roc_curve(y_test, prediccion)
plt.plot(fpr, tpr, label='ROC')
plt.plot([0, 1], [0, 1], color='black', linestyle='--', label='Aleatorio')
plt.xlabel('Falso positivo')
plt.ylabel('Verdadero positivo')
plt.title('Curva ROC')
plt.legend()
plt.show()

# Calculamos el área bajo la curva ROC
auc = roc_auc_score(y_test, prediccion)
print('Área bajo la curva ROC:', auc)
```

**Explicación:**

* Importamos las librerías necesarias.
* Cargamos los datos.
* Limpiamos los datos.
* Convertimos las variables categóricas a numéricas.
* Creamos variables ficticias para las variables categóricas.
* Dividimos los datos en entrenamiento y prueba.
* Normalizamos los datos.
* Creamos el modelo.
* Entrenamos el modelo.
* Evaluamos el modelo.
* Hacemos una predicción.
* Calculamos la matriz de confusión.
* Visualizamos la matriz de confusión.
* Calculamos la curva ROC.
* Visualizamos la curva ROC.
* Calculamos el área bajo la curva ROC.