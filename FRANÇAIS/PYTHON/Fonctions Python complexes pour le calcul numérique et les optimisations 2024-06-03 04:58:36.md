**Code Python Complexe**

**Fonction de calcul de la dérivée numérique d'une fonction**

```python
import numpy as np

def derivee_numerique(f, x, h=1e-6):
    """Calcule la dérivée numérique de la fonction f en x.

    Args:
        f (callable): Fonction dont on veut calculer la dérivée.
        x (float): Point où calculer la dérivée.
        h (float, optional): Incrément pour le calcul des différences finies.

    Returns:
        float: Dérivée numérique de f en x.
    """
    return (f(x + h) - f(x - h)) / (2 * h)
```

**Fonction de résolution d'une équation différentielle ordinaire du premier ordre par la méthode d'Euler**

```python
def euler(f, x0, y0, h, n):
    """Résout une équation différentielle ordinaire du premier ordre par la méthode d'Euler.

    Args:
        f (callable): Fonction définissant l'équation différentielle y' = f(x, y).
        x0 (float): Point initial.
        y0 (float): Valeur initiale de y.
        h (float): Pas d'intégration.
        n (int): Nombre de pas d'intégration.

    Returns:
        list: Liste des valeurs de y calculées aux pas d'intégration.
    """
    y = [y0]
    for i in range(n):
        y.append(y[-1] + h * f(x0 + i * h, y[-1]))
    return y
```

**Fonction de calcul de l'inverse d'une matrice**

```python
import numpy as np
from scipy.linalg import inv

def inv_matrice(A):
    """Calcule l'inverse d'une matrice.

    Args:
        A (numpy.ndarray): Matrice à inverser.

    Returns:
        numpy.ndarray: Inverse de la matrice A.
    """
    try:
        return inv(A)
    except np.linalg.LinAlgError:
        raise ValueError("Matrice non inversible")
```

**Fonction d'optimisation par gradient descendent**

```python
def gradient_descendant(f, x0, tol=1e-6, max_iter=1000):
    """Effectue une optimisation par gradient descendent.

    Args:
        f (callable): Fonction à optimiser.
        x0 (list): Point de départ de l'optimisation.
        tol (float, optional): Tolérance pour l'arrêt de l'optimisation.
        max_iter (int, optional): Nombre maximal d'itérations.

    Returns:
        list: Point optimal trouvé.
    """
    x = x0
    it = 0
    while it < max_iter and np.linalg.norm(np.gradient(f, x)) > tol:
        x -= np.gradient(f, x) * 0.01
        it += 1
    return x
```

**Exemple d'utilisation**

```python
# Calcul de la dérivée numérique de la fonction sin(x) en x = π/2
print(derivee_numerique(np.sin, np.pi / 2))

# Résolution de l'équation différentielle y' = x + y avec y(0) = 1
y = euler(lambda x, y: x + y, 0, 1, 0.1, 10)
print(y)

# Calcul de l'inverse de la matrice [[1, 2], [3, 4]]
A = np.array([[1, 2], [3, 4]])
print(inv_matrice(A))

# Optimisation de la fonction f(x) = x^2 + sin(x)
x0 = [1, 2]
x = gradient_descendant(lambda x: x[0]**2 + np.sin(x[1]), x0)
print(x)
```