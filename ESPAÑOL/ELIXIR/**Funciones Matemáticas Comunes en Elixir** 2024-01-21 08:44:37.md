```elixir
# Definir una función recursiva para calcular el factorial de un número.
defmodule Factorial do
  def fact(0), do: 1
  def fact(n) when n > 0, do: n * fact(n-1)
end

# Definir una función para generar una lista de números Primos.
defmodule Prime do
  def is_prime(n) when n <= 1, do: false
  def is_prime(n), do: is_prime(n, 2)

  defp is_prime(n, i) when i * i > n, do: true
  defp is_prime(n, i) when rem(n, i) == 0, do: false
  defp is_prime(n, i), do: is_prime(n, i+1)

  def generate(limit) do
    Enum.filter(1..limit, &is_prime/1)
  end
end

# Definir una función para ordenar una lista de elementos.
defmodule Sort do
  def sort(list), do: sort(list, [])

  defp sort([], sorted), do: sorted
  defp sort([head | tail], sorted) do
    sorted = Enum.sort([head | sorted])
    sort(tail, sorted)
  end
end

# Definir una función para encontrar el máximo divisor común de dos números.
defmodule GCD do
  def gcd(a, b) when a == 0, do: b
  def gcd(a, b), do: gcd(b, rem(a, b))
end

# Demostrar el uso de estas funciones.
IO.puts("Factorial de 5: #{Factorial.fact(5)}")
IO.puts("Lista de números Primos hasta 100: #{Prime.generate(100)}")
IO.puts("Ordenar la lista [5, 3, 1, 2, 4]: #{Sort.sort([5, 3, 1, 2, 4])}")
IO.puts("Máximo divisor común de 12 y 18: #{GCD.gcd(12, 18)}")
```

Explicación del código:

1. **Función `Factorial`**: Esta función calcula el factorial de un número usando la recursión. El caso base es cuando `n` es igual a 0, en cuyo caso el factorial es 1. De lo contrario, la función llama a sí misma con `n-1` y multiplica el resultado por `n`.
2. **Función `Prime`**: Esta función genera una lista de números primos hasta un límite especificado. Utiliza la función `is_prime` para comprobar si un número es primo o no. La función `is_prime` utiliza los elementos `i` de una lista llamada `Range.new(2, sqrt(n)+1)`.
3. **Función `Sort`**: Esta función ordena una lista de elementos usando el algoritmo de clasificación por inserción. El caso base es cuando la lista está vacía, en cuyo caso la lista ordenada es simplemente una lista vacía. De lo contrario, la función toma el primer elemento de la lista y lo inserta en la lista ordenada en la posición correcta.
4. **Función `GCD`**: Esta función calcula el máximo divisor común de dos números usando el algoritmo de Euclides. El caso base es cuando el primer número es igual a 0, en cuyo caso el máximo divisor común es el segundo número. De lo contrario, la función llama a sí misma con el segundo número y el resto de la división del primer número por el segundo número.

Este código demuestra el uso de funciones recursivas, funciones de primera clase, comprensión de listas y patrones de coincidencia para resolver problemas comunes en la programación.