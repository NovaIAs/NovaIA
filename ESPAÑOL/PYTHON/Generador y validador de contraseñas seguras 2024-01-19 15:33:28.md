```python
import random
import string
import time

def generar_contraseña():
  # Creamos un conjunto de caracteres posibles para la contraseña.
  caracteres = list(string.ascii_letters + string.digits + "!@#$%^&*()")

  # Elegimos una longitud aleatoria para la contraseña entre 12 y 20 caracteres.
  longitud = random.randint(12, 20)

  # Creamos una lista vacía para almacenar la contraseña.
  contraseña = []

  # Generamos la contraseña eligiendo caracteres aleatorios del conjunto de caracteres posibles.
  for i in range(longitud):
    contraseña.append(random.choice(caracteres))

  # Devolvemos la contraseña como una cadena de caracteres.
  return "".join(contraseña)

def comprobar_contraseña(contraseña):
  # Comprobamos que la contraseña tenga al menos 12 caracteres.
  if len(contraseña) < 12:
    return False

  # Comprobamos que la contraseña tenga al menos una letra mayúscula.
  if not any(char.isupper() for char in contraseña):
    return False

  # Comprobamos que la contraseña tenga al menos una letra minúscula.
  if not any(char.islower() for char in contraseña):
    return False

  # Comprobamos que la contraseña tenga al menos un dígito.
  if not any(char.isdigit() for char in contraseña):
    return False

  # Comprobamos que la contraseña tenga al menos un símbolo especial.
  if not any(char in string.punctuation for char in contraseña):
    return False

  # Si la contraseña cumple todos los requisitos, devolvemos True.
  return True

def main():
  # Generamos una contraseña aleatoria.
  contraseña = generar_contraseña()

  # Mostramos la contraseña al usuario.
  print("Tu nueva contraseña es:", contraseña)

  # Le pedimos al usuario que introduzca la contraseña de nuevo.
  contraseña_introducida = input("Introduce la contraseña de nuevo: ")

  # Comprobamos si la contraseña introducida es correcta.
  if comprobar_contraseña(contraseña_introducida):
    print("Enhorabuena, has introducido la contraseña correctamente.")
  else:
    print("Lo siento, la contraseña introducida es incorrecta.")

# Llamamos a la función main() para ejecutar el programa.
if __name__ == "__main__":
  main()
```

Explicación del código:

* La función `generar_contraseña()` genera una contraseña aleatoria de entre 12 y 20 caracteres. La contraseña se genera eligiendo caracteres aleatorios de un conjunto de caracteres posibles que incluye letras mayúsculas y minúsculas, dígitos y símbolos especiales.
* La función `comprobar_contraseña()` comprueba si una contraseña cumple los siguientes requisitos:
    * Tiene al menos 12 caracteres.
    * Tiene al menos una letra mayúscula.
    * Tiene al menos una letra minúscula.
    * Tiene al menos un dígito.
    * Tiene al menos un símbolo especial.
* La función `main()` genera una contraseña aleatoria, la muestra al usuario y le pide que la introduzca de nuevo. Si la contraseña introducida es correcta, el programa muestra un mensaje de enhorabuena. Si la contraseña introducida es incorrecta, el programa muestra un mensaje de error.

El programa se ejecuta llamando a la función `main()`.