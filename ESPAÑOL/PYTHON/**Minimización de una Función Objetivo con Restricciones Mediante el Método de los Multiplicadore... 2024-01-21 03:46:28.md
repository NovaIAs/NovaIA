```python
# Importación de bibliotecas necesarias
import numpy as np
import matplotlib.pyplot as plt
from scipy.optimize import minimize

# Definición de la función objetivo a minimizar
def funcion_objetivo(x):
    """Función objetivo a minimizar.

    Args:
        x: Vector de variables independientes.

    Returns:
        Valor de la función objetivo en x.
    """

    # Cálculo del valor de la función objetivo
    f = np.sum(x**2) - 10*np.cos(2*np.pi*x)

    return f

# Definición de las restricciones de la optimización
def restricciones(x):
    """Restricciones de la optimización.

    Args:
        x: Vector de variables independientes.

    Returns:
        Lista de restricciones.
    """

    # Restricciones de desigualdad
    g1 = x[0] - x[1]
    g2 = x[1] - x[2]

    # Restricciones de igualdad
    h1 = x[0] + x[1] + x[2] - 1

    # Devolver la lista de restricciones
    return [g1, g2, h1]

# Definición de los límites de las variables independientes
bounds = ((-5, 5), (-5, 5), (-5, 5))

# Resolución del problema de optimización
resultado = minimize(funcion_objetivo, np.array([0, 0, 0]), method='SLSQP', bounds=bounds, constraints=restricciones)

# Impresión del resultado de la optimización
print("Resultado de la optimización:")
print("Variables independientes óptimas:", resultado.x)
print("Valor óptimo de la función objetivo:", resultado.fun)

# Gráfica de la función objetivo
x = np.linspace(-5, 5, 100)
y = funcion_objetivo(x)

plt.plot(x, y)
plt.xlabel("x")
plt.ylabel("f(x)")
plt.title("Gráfica de la función objetivo")
plt.show()
```

Este código implementa un algoritmo de optimización para minimizar una función objetivo sujeta a restricciones. La función objetivo es una función cuadrática con un término coseno, y las restricciones son una restricción de desigualdad y una restricción de igualdad. El algoritmo de optimización utilizado es el método de los multiplicadores de Lagrange, que se implementa utilizando la biblioteca `scipy.optimize`.

El resultado de la optimización es un vector de tres variables independientes óptimas, que representan el punto mínimo de la función objetivo. El valor óptimo de la función objetivo es el valor de la función objetivo en el punto mínimo.

El código también incluye una gráfica de la función objetivo, que muestra cómo varía la función objetivo en función de la variable independiente.