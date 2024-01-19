```elixir
# Definimos una función que recibe como argumento una lista de números
# y devuelve la suma de todos ellos.
def suma(lista) do
  Enum.reduce(lista, 0, fn x, acc -> x + acc end)
end

# Definimos una función que recibe como argumento un número
# y devuelve su factorial.
def factorial(n) do
  cond do
    n == 0 -> 1
    n > 0 -> n * factorial(n-1)
  end
end

# Definimos una función que recibe como argumento un número
# y devuelve un booleano indicando si es primo o no.
def primo?(n) do
  cond do
    n <= 1 -> false
    n == 2 -> true
    n > 2 and rem(n, 2) == 0 -> false
    true ->
      i = 3
      while i * i <= n do
        if rem(n, i) == 0 do
          false
        end
        i += 2
      end
      true
  end
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números primos de la lista.
def primos(lista) do
  Enum.filter(lista, &primo?(&1))
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números pares de la lista.
def pares(lista) do
  Enum.filter(lista, &(&1 rem 2 == 0))
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números impares de la lista.
def impares(lista) do
  Enum.filter(lista, &(&1 rem 2 == 1))
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números perfectos de la lista.
# Un número perfecto es un número natural para el que la suma de sus divisores propios
# (es decir, todos sus divisores excepto él mismo) es igual al propio número.
def perfectos(lista) do
  Enum.filter(lista, fn n ->
    divisores = Enum.filter(1..n-1, &(&1 rem n == 0))
    suma(divisores) == n
  end)
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números amigos de la lista.
# Dos números son amigos si la suma de los divisores propios de uno
# es igual al otro número, y viceversa.
def amigos(lista) do
  Enum.filter(lista, fn n ->
    divisores = Enum.filter(1..n-1, &(&1 rem n == 0))
    suma(divisores) in lista
  end)
end

# Definimos una función que recibe como argumento una lista de números
# y devuelve la lista de los números de Carmichael de la lista.
# Un número de Carmichael es un número natural que pasa la prueba de primalidad de Fermat
# para todos los enteros positivos menores que él, pero que no es primo.
def carmichael(lista) do
  Enum.filter(lista, fn n ->
    cond do
      n <= 1 -> false
      n == 2 -> true
      n > 2 and rem(n, 2) == 0 -> false
      true ->
        i = 3
        while i * i <= n do
          if rem(n, i) == 0 do
            false
          end
          i += 2
        end
        Enum.all?(2..n-1, fn a -> pow(a, n-1, n) == 1 end)
    end
  end)
end

# Definimos una función que recibe como argumento una base,
# un exponente y un módulo, y devuelve el resultado de elevar la base
# a la potencia del exponente módulo el módulo.
def pow(base, exp, mod) do
  if exp == 0 do
    1
  else
    if rem(exp, 2) == 0 do
      t = pow(base, exp / 2, mod)
      rem(t * t, mod)
    else
      rem(base * pow(base, exp - 1, mod), mod)
    end
  end
end

# Definimos una lista de números.
lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Imprimimos la suma de la lista.
IO.puts("Suma de la lista: #{suma(lista)}")

# Imprimimos el factorial del primer número de la lista.
IO.puts("Factorial del primer número de la lista: #{factorial(lista |> hd)}")

# Imprimimos los números primos de la lista.
IO.puts("Números primos de la lista: #{primos(lista)}")

# Imprimimos los números pares de la lista.
IO.puts("Números pares de la lista: #{pares(lista)}")

# Imprimimos los números impares de la lista.
IO.puts("Números impares de la lista: #{impares(lista)}")

# Imprimimos los números perfectos de la lista.
IO.puts("Números perfectos de la lista: #{perfectos(lista)}")

# Imprimimos los números amigos de la lista.
IO.puts("Números amigos de la lista: #{amigos(lista)}")

# Imprimimos los números de Carmichael de la lista.
IO.puts("Números de Carmichael de la lista: #{carmichael(lista)}")
```

Este código es un ejemplo de código complejo en Elixir. Contiene varias funciones que realizan diferentes operaciones matemáticas y lógicas sobre una lista de números.

* La función `suma` calcula la suma de todos los elementos de una lista.
* La función `factorial` calcula el factorial de un número.
* La función `primo?` comprueba si un número es primo.
* La función `primos` devuelve una lista con todos los números primos de una lista.
* La función `pares` devuelve una lista con todos los números pares de una lista.
* La función `impares` devuelve una lista con todos los números impares de una lista.
* La función `perfectos` devuelve una lista con todos los números perfectos de una lista.
* La función `amigos` devuelve una lista con todos los números amigos de una lista.
* La función `carmichael` devuelve una lista con todos los números de Carmichael de una lista.
* La función `pow` calcula la potencia de un número módulo otro número.

El código también incluye una lista de números y una serie de llamadas a las funciones definidas anteriormente para demostrar su uso.

Este código es un ejemplo de cómo Elixir puede utilizarse para realizar operaciones matemáticas y lógicas complejas de forma sencilla y concisa.