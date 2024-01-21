```elixir
# Importamos la librería `Enum` para utilizar la función `map/2`.
import Enum

# Definimos una función `capitalizar/1` que recibe una cadena de texto y devuelve una nueva cadena
# con la primera letra en mayúscula.
def capitalizar(cadena) do
  String.capitalize(cadena)
end

# Definimos una lista de cadenas de texto.
lista = ["hola", "mundo", "!"]

# Utilizamos la función `map/2` de la librería `Enum` para aplicar la función `capitalizar/1`
# a cada elemento de la lista. El resultado es una nueva lista con todos los elementos en mayúscula.
lista_capitalizada = Enum.map(lista, &capitalizar/1)

# Imprimimos la lista capitalizada.
IO.puts(lista_capitalizada)

# Definimos una función `sumar/2` que recibe dos números y devuelve la suma de ambos.
def sumar(a, b) do
  a + b
end

# Definimos una lista de números.
numeros = [1, 2, 3, 4, 5]

# Utilizamos la función `reduce/2` de la librería `Enum` para aplicar la función `sumar/2`
# a todos los elementos de la lista. El resultado es la suma de todos los elementos de la lista.
suma = Enum.reduce(numeros, 0, &sumar/2)

# Imprimimos la suma.
IO.puts(suma)

# Definimos una función `filtrar/2` que recibe una lista y un predicado y devuelve una nueva lista
# con los elementos de la lista que cumplen el predicado.
def filtrar(lista, predicado) do
  Enum.filter(lista, predicado)
end

# Definimos un predicado que devuelve `true` si un número es par y `false` si es impar.
def par?(numero) do
  rem(numero, 2) == 0
end

# Utilizamos la función `filtrar/2` para filtrar la lista de números y quedarnos solo con los
# números pares.
numeros_pares = filtrar(numeros, &par?/1)

# Imprimimos la lista de números pares.
IO.puts(numeros_pares)
```

Explicación del código:

1. Importamos la librería `Enum` para utilizar la función `map/2`.
2. Definimos una función `capitalizar/1` que recibe una cadena de texto y devuelve una nueva cadena con la primera letra en mayúscula.
3. Definimos una lista de cadenas de texto.
4. Utilizamos la función `map/2` de la librería `Enum` para aplicar la función `capitalizar/1` a cada elemento de la lista. El resultado es una nueva lista con todos los elementos en mayúscula.
5. Imprimimos la lista capitalizada.
6. Definimos una función `sumar/2` que recibe dos números y devuelve la suma de ambos.
7. Definimos una lista de números.
8. Utilizamos la función `reduce/2` de la librería `Enum` para aplicar la función `sumar/2` a todos los elementos de la lista. El resultado es la suma de todos los elementos de la lista.
9. Imprimimos la suma.
10. Definimos una función `filtrar/2` que recibe una lista y un predicado y devuelve una nueva lista con los elementos de la lista que cumplen el predicado.
11. Definimos un predicado que devuelve `true` si un número es par y `false` si es impar.
12. Utilizamos la función `filtrar/2` para filtrar la lista de números y quedarnos solo con los números pares.
13. Imprimimos la lista de números pares.