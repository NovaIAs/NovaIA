```python
def encontrar_equilibrio(cadena):
  """
  Encuentra el punto de equilibrio en una cadena de paréntesis.

  Args:
    cadena: una cadena de paréntesis.

  Returns:
    El índice del punto de equilibrio en la cadena.
  """

  # Inicializar el punto de equilibrio.
  punto_equilibrio = 0

  # Recorrer la cadena.
  for i, caracter in enumerate(cadena):
    # Si el caracter es un paréntesis abierto, incrementar el punto de equilibrio.
    if caracter == "(":
      punto_equilibrio += 1
    # Si el caracter es un paréntesis cerrado, decrementar el punto de equilibrio.
    elif caracter == ")":
      punto_equilibrio -= 1

    # Si el punto de equilibrio es cero, hemos encontrado el punto de equilibrio.
    if punto_equilibrio == 0:
      return i

  # Si no hemos encontrado el punto de equilibrio, devolver -1.
  return -1

def prueba_encontrar_equilibrio():
  """
  Prueba la función encontrar_equilibrio.
  """

  pruebas = [
    ("()", 1),
    (")(", -1),
    ("(()())", 5),
    ("((()))", 3),
    ("(()))", -1),
    ("((())))(", -1),
  ]

  for cadena, esperado in pruebas:
    resultado = encontrar_equilibrio(cadena)
    assert resultado == esperado, f"La prueba {cadena} falló. Se esperaba {esperado}, pero se obtuvo {resultado}."

  print("Todas las pruebas pasaron.")

prueba_encontrar_equilibrio()
```

Explicación:

* La función `encontrar_equilibrio` toma una cadena de paréntesis como argumento y devuelve el índice del punto de equilibrio en la cadena.
* El punto de equilibrio es el índice donde la cadena está equilibrada, es decir, el número de paréntesis abiertos es igual al número de paréntesis cerrados.
* La función recorre la cadena y mantiene un contador del número de paréntesis abiertos y cerrados.
* Cuando el contador es cero, hemos encontrado el punto de equilibrio.
* Si llegamos al final de la cadena y el contador no es cero, significa que la cadena no está equilibrada y devolvemos -1.
* La función `prueba_encontrar_equilibrio` prueba la función `encontrar_equilibrio` con una serie de pruebas.
* La función `prueba_encontrar_equilibrio` afirma que el resultado de llamar a la función `encontrar_equilibrio` con una cadena de prueba es igual al valor esperado.
* Si todas las pruebas pasan, imprimimos un mensaje de éxito.