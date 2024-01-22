```python
def es_primo(numero: int) -> bool:
    """
    Comprueba si un número es primo.

    Parámetros:
        numero: El número a comprobar.

    Devuelve:
        True si el número es primo, False en caso contrario.
    """

    if numero <= 1:
        return False

    for i in range(2, int(numero ** 0.5) + 1):
        if numero % i == 0:
            return False

    return True


def generar_primos(limite: int) -> list:
    """
    Genera una lista de números primos hasta un límite dado.

    Parámetros:
        limite: El límite hasta el que generar los números primos.

    Devuelve:
        Una lista de números primos hasta el límite dado.
    """

    primos = []

    for numero in range(2, limite + 1):
        if es_primo(numero):
            primos.append(numero)

    return primos


def factorizar_primo(numero: int) -> list:
    """
    Factoriza un número en sus factores primos.

    Parámetros:
        numero: El número a factorizar.

    Devuelve:
        Una lista de factores primos del número dado.
    """

    factores = []

    divisor = 2

    while numero > 1:
        if numero % divisor == 0:
            factores.append(divisor)
            numero //= divisor
        else:
            divisor += 1

    return factores


def mcd(a: int, b: int) -> int:
    """
    Calcula el máximo común divisor de dos números.

    Parámetros:
        a: El primer número.
        b: El segundo número.

    Devuelve:
        El máximo común divisor de los dos números dados.
    """

    while b:
        a, b = b, a % b

    return a


def mcm(a: int, b: int) -> int:
    """
    Calcula el mínimo común múltiplo de dos números.

    Parámetros:
        a: El primer número.
        b: El segundo número.

    Devuelve:
        El mínimo común múltiplo de los dos números dados.
    """

    return a * b // mcd(a, b)


def suma_digitos(numero: int) -> int:
    """
    Calcula la suma de los dígitos de un número.

    Parámetros:
        numero: El número del que se quiere sumar los dígitos.

    Devuelve:
        La suma de los dígitos del número dado.
    """

    suma = 0

    while numero > 0:
        suma += numero % 10
        numero //= 10

    return suma


def es_perfecto(numero: int) -> bool:
    """
    Comprueba si un número es perfecto.

    Parámetros:
        numero: El número a comprobar.

    Devuelve:
        True si el número es perfecto, False en caso contrario.
    """

    if numero <= 0:
        return False

    suma = 0

    for divisor in range(1, int(numero ** 0.5) + 1):
        if numero % divisor == 0:
            suma += divisor
            if numero // divisor != divisor:
                suma += numero // divisor

    return suma == numero


def es_abundante(numero: int) -> bool:
    """
    Comprueba si un número es abundante.

    Parámetros:
        numero: El número a comprobar.

    Devuelve:
        True si el número es abundante, False en caso contrario.
    """

    if numero <= 0:
        return False

    suma = 0

    for divisor in range(1, int(numero ** 0.5) + 1):
        if numero % divisor == 0:
            suma += divisor
            if numero // divisor != divisor:
                suma += numero // divisor

    return suma > numero


def es_deficiente(numero: int) -> bool:
    """
    Comprueba si un número es deficiente.

    Parámetros:
        numero: El número a comprobar.

    Devuelve:
        True si el número es deficiente, False en caso contrario.
    """

    if numero <= 0:
        return False

    suma = 0

    for divisor in range(1, int(numero ** 0.5) + 1):
        if numero % divisor == 0:
            suma += divisor
            if numero // divisor != divisor:
                suma += numero // divisor

    return suma < numero
```

Este código implementa una serie de funciones matemáticas útiles, incluyendo funciones para comprobar si un número es primo, generar una lista de números primos, factorizar un número en sus factores primos, calcular el máximo común divisor y el mínimo común múltiplo de dos números, sumar los dígitos de un número, comprobar si un número es perfecto, abundante o deficiente.