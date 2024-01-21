```python
# Importar las bibliotecas necesarias.
import numpy as np
import matplotlib.pyplot as plt
from sklearn.datasets import load_iris
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsClassifier

# Cargar el conjunto de datos Iris.
iris = load_iris()

# Dividir el conjunto de datos en conjuntos de entrenamiento y prueba.
X_train, X_test, y_train, y_test = train_test_split(iris.data, iris.target, test_size=0.2, random_state=42)

# Crear un clasificador KNN con k = 3.
knn = KNeighborsClassifier(n_neighbors=3)

# Entrenar el clasificador con el conjunto de entrenamiento.
knn.fit(X_train, y_train)

# Predecir las etiquetas de las muestras del conjunto de prueba.
y_pred = knn.predict(X_test)

# Calcular la precisión del clasificador.
accuracy = np.mean(y_pred == y_test)

# Imprimir la precisión del clasificador.
print("La precisión del clasificador es:", accuracy)

# Crear una gráfica de dispersión de los datos.
plt.scatter(X_train[:, 0], X_train[:, 1], c=y_train, cmap=plt.cm.coolwarm)
plt.scatter(X_test[:, 0], X_test[:, 1], c=y_test, cmap=plt.cm.coolwarm)

# Dibujar los límites de decisión del clasificador.
plt.contourf(X_train[:, 0], X_train[:, 1], knn.predict(X_train).reshape(X_train.shape[0], -1), alpha=0.2, cmap=plt.cm.coolwarm)
plt.show()
```

Explicación del código:

1. Importar las bibliotecas necesarias.
2. Cargar el conjunto de datos Iris.
3. Dividir el conjunto de datos en conjuntos de entrenamiento y prueba.
4. Crear un clasificador KNN con k = 3.
5. Entrenar el clasificador con el conjunto de entrenamiento.
6. Predecir las etiquetas de las muestras del conjunto de prueba.
7. Calcular la precisión del clasificador.
8. Imprimir la precisión del clasificador.
9. Crear una gráfica de dispersión de los datos.
10. Dibujar los límites de decisión del clasificador.