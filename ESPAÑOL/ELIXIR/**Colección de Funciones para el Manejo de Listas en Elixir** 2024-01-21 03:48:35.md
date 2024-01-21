```elixir
defmodule HolaMundo do
  def hello(nombre) do
    "Hola, #{nombre}!"
  end

  def suma(a, b) do
    a + b
  end

  def resta(a, b) do
    a - b
  end

  def multiplica(a, b) do
    a * b
  end

  def divide(a, b) do
    a / b
  end

  def potencia(a, b) do
    :math.pow(a, b)
  end

  def factorial(n) do
    Enum.reduce(1..n, 1, fn x, acc -> x * acc end)
  end

  def fibonacci(n) do
    Enum.reduce(1..n, {0, 1}, fn _, {a, b} -> {b, a + b} end) |> elem(1)
  end

  def es_primo(n) do
    Enum.all?(2..(n-1), fn x -> rem(n, x) != 0 end)
  end

  def lista_primos(n) do
    Enum.filter(1..n, &es_primo(&1))
  end

  def ordena_lista(lista) do
    Enum.sort(lista)
  end

  def invierte_lista(lista) do
    Enum.reverse(lista)
  end

  def junta_listas(lista1, lista2) do
    Enum.concat(lista1, lista2)
  end

  def elimina_repetidos(lista) do
    Enum.uniq(lista)
  end

  def busca_elemento(lista, elemento) do
    Enum.find_index(lista, &(&1 == elemento))
  end

  def elimina_elemento(lista, elemento) do
    Enum.filter(lista, &(&1 != elemento))
  end

  def añade_elemento(lista, elemento) do
    Enum.concat(lista, [elemento])
  end

  def sustituye_elemento(lista, elemento_viejo, elemento_nuevo) do
    Enum.map(lista, fn x ->
      if x == elemento_viejo do
        elemento_nuevo
      else
        x
      end
    end)
  end

  def rota_lista(lista, n) do
    Enum.slice(lista, n..-1) ++ Enum.slice(lista, 0..n-1)
  end

  def mezcla_listas(lista1, lista2) do
    Enum.zip_with(lista1, lista2, fn x, y -> [x, y] end) |> Enum.flat_map(&1)
  end

  def agrupa_lista(lista, n) do
    Enum.chunk(lista, n)
  end

  def filtra_lista(lista, condicion) do
    Enum.filter(lista, condicion)
  end

  def mapea_lista(lista, funcion) do
    Enum.map(lista, funcion)
  end

  def reduce_lista(lista, inicial, funcion) do
    Enum.reduce(lista, inicial, funcion)
  end
end
```

Este código Elixir proporciona un conjunto completo de funciones para manipular y procesar listas de datos. Incluye funciones para sumar, restar, multiplicar, dividir, elevar a la potencia, calcular factoriales y números de Fibonacci, determinar si un número es primo, generar listas de números primos, ordenar listas, invertir listas, concatenar listas, eliminar elementos duplicados, buscar elementos en listas, eliminar elementos de listas, añadir elementos a listas, sustituir elementos en listas, rotar listas, mezclar listas, agrupar listas, filtrar listas, mapear listas y reducir listas.