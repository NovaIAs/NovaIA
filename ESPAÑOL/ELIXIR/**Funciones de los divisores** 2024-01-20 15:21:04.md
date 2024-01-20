```elixir
# Definimos una función que recibe un número entero `n` como parámetro
def suma_divisores(n) do
  # Utilizamos `Enum.reduce/3` para sumar los divisores de `n`
  Enum.reduce(1..n, 0, fn(i, suma) ->
    # Si `i` es un divisor de `n`, lo añadimos a la suma
    if rem(n, i) == 0, do: suma + i, else: suma
  end)
end

# Imprimimos la suma de los divisores de los números del 1 al 100
IO.puts "Suma de los divisores de los números del 1 al 100:"
Enum.each(1..100, fn(n) -> IO.puts "#{n}: #{suma_divisores(n)}" end)

# Definimos una función que recibe una lista de números enteros `lista` como parámetro
def lista_divisores(lista) do
  # Utilizamos `Enum.flat_map/2` para obtener una lista con todos los divisores de cada número en `lista`
  Enum.flat_map(lista, fn(n) -> Enum.filter(1..n, fn(i) -> rem(n, i) == 0 end) end)
end

# Imprimimos la lista de divisores de los números del 1 al 100
IO.puts "Lista de divisores de los números del 1 al 100:"
Enum.each(1..100, fn(n) -> IO.puts "#{n}: #{lista_divisores([n])}" end)

# Definimos una función que recibe una lista de números enteros `lista` como parámetro
def suma_divisores_lista(lista) do
  # Utilizamos `Enum.reduce/3` para sumar los divisores de cada número en `lista`
  Enum.reduce(lista, 0, fn(n, suma) -> suma + suma_divisores(n) end)
end

# Imprimimos la suma de los divisores de los números del 1 al 100
IO.puts "Suma de los divisores de los números del 1 al 100:"
IO.puts suma_divisores_lista(1..100)
```

Este código es una implementación de las siguientes funciones:

* `suma_divisores/1`: Esta función recibe un número entero `n` como parámetro y devuelve la suma de sus divisores.
* `lista_divisores/1`: Esta función recibe una lista de números enteros `lista` como parámetro y devuelve una lista con todos los divisores de cada número en `lista`.
* `suma_divisores_lista/1`: Esta función recibe una lista de números enteros `lista` como parámetro y devuelve la suma de los divisores de todos los números en `lista`.

El código también imprime los resultados de estas funciones para los números del 1 al 100.