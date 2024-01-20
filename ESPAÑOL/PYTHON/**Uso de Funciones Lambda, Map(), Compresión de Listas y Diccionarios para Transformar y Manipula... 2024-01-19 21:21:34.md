```python
# Creación de una tupla con una lista de números
numeros = (1, 2, 3, 4, 5)

# Uso de una expresión lambda para crear una función que calcula el cuadrado de un número
cuadrado = lambda x: x ** 2

# Uso de la función map() para aplicar la función cuadrado a cada elemento de la tupla
numeros_al_cuadrado = map(cuadrado, numeros)

# Conversión de la lista de resultados en una tupla
numeros_al_cuadrado_tupla = tuple(numeros_al_cuadrado)

# Creación de un diccionario con claves y valores numéricos
diccionario = {1: 'uno', 2: 'dos', 3: 'tres', 4: 'cuatro', 5: 'cinco'}

# Uso de una comprensión de diccionario para crear un nuevo diccionario con las claves y valores invertidos
diccionario_invertido = {v: k for k, v in diccionario.items()}

# Uso de la función zip() para crear una lista de pares de elementos de las dos tuplas
pares = zip(numeros, numeros_al_cuadrado_tupla)

# Uso de una comprensión de listas para crear una lista de cadenas que representan los pares de elementos
cadenas_pares = [f'{numero} - {cuadrado}' for numero, cuadrado in pares]

# Uso de la función join() para crear una cadena con las cadenas_pares separadas por comas
cadena_final = ', '.join(cadenas_pares)

# Impresión de la cadena final
print(cadena_final)
```

Este código realiza una serie de operaciones utilizando varias características avanzadas de Python, incluyendo expresiones lambda, la función map(), comprensión de listas y diccionarios. El código comienza creando una tupla con una lista de números y luego utiliza una expresión lambda para crear una función que calcula el cuadrado de un número. Luego, la función map() se utiliza para aplicar la función cuadrado a cada elemento de la tupla, resultando en una lista de números cuadrados. La lista de resultados se convierte en una tupla y se crea un diccionario con claves y valores numéricos. Una comprensión de diccionario se utiliza para crear un nuevo diccionario con las claves y valores invertidos. La función zip() se utiliza para crear una lista de pares de elementos de las dos tuplas, y una comprensión de listas se utiliza para crear una lista de cadenas que representan los pares de elementos. Finalmente, la función join() se utiliza para crear una cadena con las cadenas_pares separadas por comas. La cadena final se imprime en la consola.