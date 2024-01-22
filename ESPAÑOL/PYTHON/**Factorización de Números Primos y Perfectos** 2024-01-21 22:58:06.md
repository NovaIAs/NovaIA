```
def es_primo(numero):
    """
    Comprueba si un número es primo.

    Args:
    numero: El número a comprobar.

    Returns:
    True si el número es primo, False en caso contrario.
    """

    # Si el número es 1, no es primo
    if numero == 1:
        return False

    # Si el número es divisible por 2, no es primo
    if numero % 2 == 0:
        return False

    # Iteramos sobre todos los números impares hasta la raíz cuadrada del número
    for i in range(3, int(numero ** 0.5) + 1, 2):
        # Si el número es divisible por algún número impar, no es primo
        if numero % i == 0:
            return False

    # Si el número no es divisible por ningún número hasta su raíz cuadrada, es primo
    return True


def generar_primos(limite):
    """
    Genera una lista de todos los números primos hasta un límite dado.

    Args:
    limite: El límite hasta el cual generar números primos.

    Returns:
    Una lista de todos los números primos hasta el límite dado.
    """

    # Creamos una lista vacía para almacenar los números primos
    primos = []

    # Iteramos sobre todos los números desde 2 hasta el límite
    for numero in range(2, limite + 1):
        # Si el número es primo, lo añadimos a la lista
        if es_primo(numero):
            primos.append(numero)

    # Devolvemos la lista de números primos
    return primos


def factorizar(numero):
    """
    Factoriza un número en sus factores primos.

    Args:
    numero: El número a factorizar.

    Returns:
    Una lista con los factores primos del número dado.
    """

    # Creamos una lista vacía para almacenar los factores primos
    factores = []

    # Mientras el número sea mayor que 1, seguimos factorizando
    while numero > 1:
        # Encontramos el factor primo más pequeño del número
        factor = 2
        while numero % factor != 0:
            factor += 1

        # Añadimos el factor primo a la lista
        factores.append(factor)

        # Dividimos el número por el factor primo
        numero //= factor

    # Devolvemos la lista de factores primos
    return factores


def es_perfecto(numero):
    """
    Comprueba si un número es perfecto.

    Args:
    numero: El número a comprobar.

    Returns:
    True si el número es perfecto, False en caso contrario.
    """

    # Si el número es 1, no es perfecto
    if numero == 1:
        return False

    # Calculamos la suma de los divisores propios del número
    suma_divisores = 0
    for i in range(1, numero):
        if numero % i == 0:
            suma_divisores += i

    # Si la suma de los divisores propios es igual al número, el número es perfecto
    return suma_divisores == numero

```

Explicación del código:

* La función `es_primo` comprueba si un número es primo. Lo hace iterando sobre todos los números desde 2 hasta la raíz cuadrada del número y comprobando si el número es divisible por alguno de ellos. Si el número es divisible por alguno de ellos, no es primo. Si el número no es divisible por ninguno de ellos, es primo.
* La función `generar_primos` genera una lista de todos los números primos hasta un límite dado. Lo hace llamando a la función `es_primo` para cada número desde 2 hasta el límite y añadiendo a la lista los números que son primos.
* La función `factorizar` factoriza un número en sus factores primos. Lo hace encontrando el factor primo más pequeño del número y dividiendo el número por ese factor primo. Luego, repite este proceso hasta que el número sea 1. Los factores primos del número son los factores primos encontrados en cada paso.
* La función `es_perfecto` comprueba si un número es perfecto. Lo hace calculando la suma de los divisores propios del número. Si la suma de los divisores propios es igual al número, el número es perfecto.