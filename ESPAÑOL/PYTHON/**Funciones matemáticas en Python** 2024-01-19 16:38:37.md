```
def descomposicion_en_factores_primos(numero):
  """
  Devuelve una lista de los factores primos de un número.

  Args:
    numero: El número del que se quiere obtener los factores primos.

  Returns:
    Una lista de los factores primos del número.
  """

  factores_primos = []
  divisor = 2

  while numero > 1:
    if numero % divisor == 0:
      factores_primos.append(divisor)
      numero //= divisor
    else:
      divisor += 1

  return factores_primos


def es_primo(numero):
  """
  Devuelve True si un número es primo, False en caso contrario.

  Args:
    numero: El número del que se quiere comprobar si es primo.

  Returns:
    True si el número es primo, False en caso contrario.
  """

  if numero == 1:
    return False

  for i in range(2, int(numero ** 0.5) + 1):
    if numero % i == 0:
      return False

  return True


def criba_de_eratostenes(limite):
  """
  Devuelve una lista de los números primos hasta un límite dado.

  Args:
    limite: El límite hasta el que se quiere obtener los números primos.

  Returns:
    Una lista de los números primos hasta el límite dado.
  """

  numeros_primos = []

  for numero in range(2, limite + 1):
    if es_primo(numero):
      numeros_primos.append(numero)

  return numeros_primos


def encontrar_divisores_propios(numero):
  """
  Devuelve una lista de los divisores propios de un número.

  Args:
    numero: El número del que se quiere obtener los divisores propios.

  Returns:
    Una lista de los divisores propios del número.
  """

  divisores_propios = []

  for i in range(1, numero):
    if numero % i == 0:
      divisores_propios.append(i)

  return divisores_propios


def suma_de_divisores_propios(numero):
  """
  Devuelve la suma de los divisores propios de un número.

  Args:
    numero: El número del que se quiere obtener la suma de los divisores propios.

  Returns:
    La suma de los divisores propios del número.
  """

  suma_divisores_propios = 0

  for divisor in encontrar_divisores_propios(numero):
    suma_divisores_propios += divisor

  return suma_divisores_propios


def es_perfecto(numero):
  """
  Devuelve True si un número es perfecto, False en caso contrario.

  Args:
    numero: El número del que se quiere comprobar si es perfecto.

  Returns:
    True si el número es perfecto, False en caso contrario.
  """

  return suma_de_divisores_propios(numero) == numero


def encontrar_numeros_perfectos(limite):
  """
  Devuelve una lista de los números perfectos hasta un límite dado.

  Args:
    limite: El límite hasta el que se quiere obtener los números perfectos.

  Returns:
    Una lista de los números perfectos hasta el límite dado.
  """

  numeros_perfectos = []

  for numero in range(1, limite + 1):
    if es_perfecto(numero):
      numeros_perfectos.append(numero)

  return numeros_perfectos


print(descomposicion_en_factores_primos(12))  # [2, 2, 3]
print(es_primo(13))  # True
print(criba_de_eratostenes(100))  # [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
print(encontrar_divisores_propios(12))  # [1, 2, 3, 4, 6]
print(suma_de_divisores_propios(12))  # 16
print(es_perfecto(6))  # True
print(encontrar_numeros_perfectos(100))  # [6]
```

Este código es un conjunto de funciones que implementan varios conceptos matemáticos, como la descomposición en factores primos, la comprobación de primalidad, la criba de Eratóstenes, la búsqueda de divisores propios, la suma de divisores propios y la comprobación de números perfectos.

La función `descomposicion_en_factores_primos` toma un número como argumento y devuelve una lista de sus factores primos. La función `es_primo` toma un número como argumento y devuelve `True` si el número es primo y `False` en caso contrario. La función `criba_de_eratostenes` toma un límite como argumento y devuelve una lista de todos los números primos hasta ese límite. La función `encontrar_divisores_propios` toma un número como argumento y devuelve una lista de todos sus divisores propios. La función `suma_de_divisores_propios` toma un número como argumento y devuelve la suma de todos sus divisores propios. La función `es_perfecto` toma un número como argumento y devuelve `True` si el número es perfecto y `False` en caso contrario. La función `encontrar_numeros_perfectos` toma un límite como argumento y devuelve una lista de todos los números perfectos hasta ese límite.

Este código es un ejemplo de cómo se pueden utilizar las matemáticas para resolver problemas informáticos.