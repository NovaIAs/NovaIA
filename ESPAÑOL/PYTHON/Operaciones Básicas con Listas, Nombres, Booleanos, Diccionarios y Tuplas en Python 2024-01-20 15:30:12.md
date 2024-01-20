```python
# Definición de variables
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
lista_nombres = ["Juan", "María", "Pedro", "Ana", "José"]
lista_booleanos = [True, False, True, True, False]
diccionario = {
    "nombre": "Juan",
    "apellido": "García",
    "edad": 25,
    "profesión": "Ingeniero"
}
tupla = (1, 2, 3, 4, 5)

# Operaciones con listas
suma_lista_numeros = sum(lista_numeros)  # Suma los elementos de la lista
promedio_lista_numeros = suma_lista_numeros / len(lista_numeros)  # Calcula el promedio de la lista
lista_pares = [numero for numero in lista_numeros if numero % 2 == 0]  # Filtra los números pares de la lista
lista_nombres_ordenados = sorted(lista_nombres)  # Ordena los nombres de la lista
lista_nombres_invertida = lista_nombres[::-1]  # Invierte el orden de los nombres de la lista

# Operaciones con nombres
nombre_completo = " ".join(lista_nombres)  # Concatena los nombres de la lista con un espacio en medio
nombre_primer_caracter = nombre_completo[0]  # Obtiene el primer carácter del nombre completo
nombre_ultimo_caracter = nombre_completo[-1]  # Obtiene el último carácter del nombre completo
nombre_sin_espacios = nombre_completo.replace(" ", "")  # Elimina los espacios del nombre completo

# Operaciones con booleanos
resultado_and = lista_booleanos[0] and lista_booleanos[1]  # Realiza la operación AND entre dos valores booleanos
resultado_or = lista_booleanos[2] or lista_booleanos[3]  # Realiza la operación OR entre dos valores booleanos
resultado_not = not lista_booleanos[4]  # Realiza la operación NOT sobre un valor booleano

# Operaciones con diccionarios
valor_nombre = diccionario["nombre"]  # Obtiene el valor de la clave "nombre" del diccionario
valor_edad = diccionario.get("edad")  # Obtiene el valor de la clave "edad" del diccionario
diccionario["profesión"] = "Ingeniero de Software"  # Actualiza el valor de la clave "profesión" en el diccionario
del diccionario["apellido"]  # Elimina la clave "apellido" del diccionario

# Operaciones con tuplas
primer_elemento_tupla = tupla[0]  # Obtiene el primer elemento de la tupla
ultimo_elemento_tupla = tupla[-1]  # Obtiene el último elemento de la tupla
tupla_invertida = tupla[::-1]  # Invierte el orden de los elementos de la tupla

# Impresión de los resultados
print("Suma de la lista de números:", suma_lista_numeros)
print("Promedio de la lista de números:", promedio_lista_numeros)
print("Lista de números pares:", lista_pares)
print("Lista de nombres ordenados:", lista_nombres_ordenados)
print("Lista de nombres invertida:", lista_nombres_invertida)

print("Nombre completo:", nombre_completo)
print("Primer carácter del nombre completo:", nombre_primer_caracter)
print("Último carácter del nombre completo:", nombre_ultimo_caracter)
print("Nombre completo sin espacios:", nombre_sin_espacios)

print("Resultado de la operación AND:", resultado_and)
print("Resultado de la operación OR:", resultado_or)
print("Resultado de la operación NOT:", resultado_not)

print("Valor de la clave 'nombre' en el diccionario:", valor_nombre)
print("Valor de la clave 'edad' en el diccionario:", valor_edad)

print("Primer elemento de la tupla:", primer_elemento_tupla)
print("Último elemento de la tupla:", ultimo_elemento_tupla)
print("Tupla invertida:", tupla_invertida)
```

Explicación del código:

* **Definición de variables:** Se definen varias variables con diferentes tipos de datos, incluyendo listas, nombres, booleanos, diccionarios y tuplas.
* **Operaciones con listas:** Se realizan varias operaciones con las listas definidas, incluyendo la suma de sus elementos, el cálculo del promedio, el filtrado de elementos, el ordenamiento y la inversión del orden de los elementos.
* **Operaciones con nombres:** Se realizan varias operaciones con los nombres definidos, incluyendo la concatenación de los nombres con un espacio en medio, la obtención del primer y último carácter de los nombres, y la eliminación de los espacios de los nombres.
* **Operaciones con booleanos:** Se realizan varias operaciones con los booleanos definidos, incluyendo la operación AND, la operación OR y la operación NOT.
* **Operaciones con diccionarios:** Se realizan varias operaciones con el diccionario definido, incluyendo la obtención del valor de una clave, la actualización del valor de una clave y la eliminación de una clave.
* **Operaciones con tuplas:** Se realizan varias operaciones con la tupla definida, incluyendo la obtención del primer y último elemento de la tupla, y la inversión del orden de los elementos de la tupla.
* **Impresión de los resultados:** Se imprimen los resultados de las operaciones realizadas con las diferentes variables.