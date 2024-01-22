```python
# Primer Bloque #

# Definir variables
num1 = 10
num2 = 20
texto = "Hola mundo!"

# Imprimir variables
print("Variables numéricas:", num1, num2)
print("Variable texto:", texto)

# Realizar operaciones matemáticas
suma = num1 + num2
resta = num2 - num1
producto = num1 * num2
cociente = num2 / num1

# Imprimir resultados de operaciones matemáticas
print("Suma:", suma)
print("Resta:", resta)
print("Producto:", producto)
print("Cociente:", cociente)

# Convertir números a texto
num1_texto = str(num1)
num2_texto = str(num2)

# Concatenar texto y números
texto_completo = texto + " " + num1_texto + " " + num2_texto

# Imprimir texto completo
print("Texto completo:", texto_completo)

# Segundo Bloque #

# Crear lista de números
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Imprimir lista de números
print("Lista de números:", lista_numeros)

# Añadir elemento a la lista
lista_numeros.append(11)

# Imprimir lista de números actualizada
print("Lista de números actualizada:", lista_numeros)

# Eliminar elemento de la lista
lista_numeros.remove(5)

# Imprimir lista de números actualizada
print("Lista de números actualizada:", lista_numeros)

# Ordenar lista de números
lista_numeros.sort()

# Imprimir lista de números ordenada
print("Lista de números ordenada:", lista_numeros)

# Invertir lista de números
lista_numeros.reverse()

# Imprimir lista de números invertida
print("Lista de números invertida:", lista_numeros)

# Tercer Bloque #

# Crear diccionario de palabras
diccionario_palabras = {"uno": 1, "dos": 2, "tres": 3, "cuatro": 4, "cinco": 5}

# Imprimir diccionario de palabras
print("Diccionario de palabras:", diccionario_palabras)

# Añadir palabra al diccionario
diccionario_palabras["seis"] = 6

# Imprimir diccionario de palabras actualizado
print("Diccionario de palabras actualizado:", diccionario_palabras)

# Eliminar palabra del diccionario
del diccionario_palabras["tres"]

# Imprimir diccionario de palabras actualizado
print("Diccionario de palabras actualizado:", diccionario_palabras)

# Comprobar si una palabra existe en el diccionario
if "cuatro" in diccionario_palabras:
    print("La palabra 'cuatro' existe en el diccionario")
else:
    print("La palabra 'cuatro' no existe en el diccionario")

# Cuarto Bloque #

# Crear función para calcular el factorial de un número
def factorial(numero):
    if numero == 0:
        return 1
    else:
        return numero * factorial(numero - 1)

# Imprimir factorial de un número
print("Factorial de 5:", factorial(5))

# Crear función para calcular el máximo común divisor de dos números
def maximo_comun_divisor(num1, num2):
    while num1 != num2:
        if num1 > num2:
            num1 = num1 - num2
        else:
            num2 = num2 - num1

    return num1

# Imprimir máximo común divisor de dos números
print("Máximo común divisor de 12 y 18:", maximo_comun_divisor(12, 18))

# Quinto Bloque #

# Importar librería de expresiones regulares
import re

# Crear expresión regular para validar una dirección de correo electrónico
expresion_regular_email = "^[a-z0-9._%+-]+@[a-z0-9.-]+\.[a-z]{2,6}$"

# Comprobar si una dirección de correo electrónico es válida
if re.match(expresion_regular_email, "nombre@ejemplo.com"):
    print("La dirección de correo electrónico es válida")
else:
    print("La dirección de correo electrónico no es válida")

# Crear expresión regular para validar un número de teléfono
expresion_regular_telefono = "^[0-9]{9}$"

# Comprobar si un número de teléfono es válido
if re.match(expresion_regular_telefono, "123456789"):
    print("El número de teléfono es válido")
else:
    print("El número de teléfono no es válido")

# Explicación del código:

* El código está dividido en cinco bloques. Cada bloque contiene una sección diferente del código.
* El primer bloque define variables, realiza operaciones matemáticas y convierte números a texto.
* El segundo bloque crea una lista de números, añade, elimina y ordena elementos de la lista, e invierte la lista.
* El tercer bloque crea un diccionario de palabras, añade, elimina y comprueba palabras en el diccionario.
* El cuarto bloque define funciones para calcular el factorial de un número y el máximo común divisor de dos números.
* El quinto bloque importa la librería de expresiones regulares, crea expresiones regulares para validar una dirección de correo electrónico y un número de teléfono, y comprueba si las entradas corresponden a los formatos correctos.