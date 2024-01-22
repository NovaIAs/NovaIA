```python
# Importamos las librerías necesarias
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize

# Definimos la función objetivo a minimizar
def funcion_objetivo(x):
    """
    Función objetivo a minimizar.

    Args:
        x: Vector de variables independientes.

    Returns:
        Valor de la función objetivo en el punto x.
    """
    return np.sum((x - np.array([1, 2, 3]))**2)

# Definimos las restricciones
def restricciones(x):
    """
    Restricciones de la función objetivo.

    Args:
        x: Vector de variables independientes.

    Returns:
        Vector de restricciones.
    """
    return np.array([x[0] + x[1] - 2, x[1] + x[2] - 4])

# Definimos los límites de las variables
def limites(x):
    """
    Límites de las variables independientes.

    Args:
        x: Vector de variables independientes.

    Returns:
        Vector de límites.
    """
    return np.array([(0, 4), (0, 4), (0, 4)])

# Resolvemos el problema de minimización
resultado = minimize(funcion_objetivo, np.array([0, 0, 0]), method='SLSQP', constraints=restricciones, bounds=limites)

# Imprimimos el resultado
print("Solución:")
print(resultado.x)

# Dibujamos la función objetivo y las restricciones
x = np.linspace(0, 4, 100)
y = np.linspace(0, 4, 100)
X, Y = np.meshgrid(x, y)
Z = funcion_objetivo(np.array([X, Y, np.zeros((100, 100))]))

plt.figure()
plt.contourf(X, Y, Z, 20, cmap='jet')
plt.plot(resultado.x[0], resultado.x[1], 'ro')
plt.plot(resultado.x[0], resultado.x[2], 'ro')
plt.plot(resultado.x[1], resultado.x[2], 'ro')
plt.xlabel('x')
plt.ylabel('y')
plt.title('Función objetivo y restricciones')
plt.show()
```

Este código resuelve un problema de minimización con restricciones utilizando el método SLSQP de la librería scipy.optimize.

La función objetivo a minimizar es la suma de los cuadrados de las diferencias entre las variables independientes y el vector [1, 2, 3].

Las restricciones son dos ecuaciones lineales que las variables independientes deben satisfacer.

Los límites de las variables independientes son [0, 4] para las tres variables.

El resultado de la minimización es el vector de variables independientes que minimiza la función objetivo y satisface las restricciones.

El código también dibuja la función objetivo y las restricciones en un gráfico de contorno.