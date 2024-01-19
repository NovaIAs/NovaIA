```python
# Definición de la función principal para calcular la Ecuación Cuadrática
def ecuacion_cuadratica(a, b, c):
    """
    Esta función calcula las raíces de una ecuación cuadrática de la forma ax^2 + bx + c = 0.

    Args:
        a (float): Coeficiente cuadrático de la ecuación.
        b (float): Coeficiente lineal de la ecuación.
        c (float): Término independiente de la ecuación.

    Returns:
        tuple: Una tupla conteniendo las dos raíces de la ecuación.
    """

    # Cálculo del discriminante
    discriminante = (b**2) - (4 * a * c)

    # Comprobación del tipo de raíces en función del discriminante
    if discriminante > 0:
        # Raíces reales y distintas
        raiz1 = (-b + (discriminante**0.5)) / (2 * a)
        raiz2 = (-b - (discriminante**0.5)) / (2 * a)
        return raiz1, raiz2
    elif discriminante == 0:
        # Raíz real única
        raiz = -b / (2 * a)
        return raiz, raiz
    else:
        # Raíces complejas conjugadas
        parte_real = -b / (2 * a)
        parte_imaginaria = ((-discriminante)**0.5) / (2 * a)
        raiz1 = parte_real + parte_imaginaria * 1j
        raiz2 = parte_real - parte_imaginaria * 1j
        return raiz1, raiz2


# Definición de la función principal para encontrar las raíces de una Ecuación Cúbica
def ecuacion_cubica(a, b, c, d):
    """
    Esta función calcula las raíces de una ecuación cúbica de la forma ax^3 + bx^2 + cx + d = 0.

    Args:
        a (float): Coeficiente cúbico de la ecuación.
        b (float): Coeficiente cuadrático de la ecuación.
        c (float): Coeficiente lineal de la ecuación.
        d (float): Término independiente de la ecuación.

    Returns:
        tuple: Una tupla conteniendo las tres raíces de la ecuación.
    """

    # Cálculo del discriminante
    discriminante = (b**2) - (3 * a * c)

    # Comprobación del tipo de raíces en función del discriminante
    if discriminante > 0:
        # Raíces reales y distintas
        raiz1 = (-b + (discriminante**0.5)) / (3 * a)
        raiz2 = (-b - (discriminante**0.5)) / (3 * a)
        raiz3 = -c / a
        return raiz1, raiz2, raiz3
    elif discriminante == 0:
        # Raíz real única
        raiz = -b / (3 * a)
        return raiz, raiz, raiz
    else:
        # Raíces complejas conjugadas
        parte_real = -b / (3 * a)
        parte_imaginaria = ((-discriminante)**0.5) / (3 * a)
        raiz1 = parte_real + parte_imaginaria * 1j
        raiz2 = parte_real - parte_imaginaria * 1j
        raiz3 = -c / a
        return raiz1, raiz2, raiz3


# Definición de la función para calcular raíces complejas
def raices_complejas(a, b, c, d):
    """
    Esta función calcula las raíces complejas de una ecuación de cuarto grado de la forma ax^4 + bx^3 + cx^2 + dx + e = 0.

    Args:
        a (float): Coeficiente de cuarto grado de la ecuación.
        b (float): Coeficiente cúbico de la ecuación.
        c (float): Coeficiente cuadrático de la ecuación.
        d (float): Coeficiente lineal de la ecuación.
        e (float): Término independiente de la ecuación.

    Returns:
        tuple: Una tupla conteniendo las cuatro raíces complejas de la ecuación.
    """

    # Cálculo del discriminante
    discriminante = (b**2) - (4 * a * c)

    # Comprobación del tipo de raíces en función del discriminante
    if discriminante > 0:
        # Raíces reales y distintas
        raiz1 = (-b + (discriminante**0.5)) / (2 * a)
        raiz2 = (-b - (discriminante**0.5)) / (2 * a)
        raiz3 = (-b + (discriminante**0.5)) / (2 * a)
        raiz4 = (-b - (discriminante**0.5)) / (2 * a)
        return raiz1, raiz2, raiz3, raiz4
    elif discriminante == 0:
        # Raíz real única
        raiz = -b / (2 * a)
        return raiz, raiz, raiz, raiz
    else:
        # Raíces complejas conjugadas
        parte_real = -b / (2 * a)
        parte_imaginaria = ((-discriminante)**0.5) / (2 * a)
        raiz1 =