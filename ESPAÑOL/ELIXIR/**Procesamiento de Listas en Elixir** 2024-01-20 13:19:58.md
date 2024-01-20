```elixir
defmodule MiModulo do
  def crear_lista(n) do
    [Enum.random(1..100) | crear_lista(n-1)]
  end

  def imprimir_lista(lista) do
    Enum.each(lista, fn elemento -> IO.puts elemento end)
  end

  def buscar_elemento(elemento, lista) do
    Enum.find_index(lista, fn x -> x == elemento end)
  end

  def ordenar_lista(lista) do
    Enum.sort(lista)
  end

  def calcular_promedio(lista) do
    Enum.sum(lista) / Enum.count(lista)
  end

  def main do
    lista = crear_lista(10)
    imprimir_lista(lista)

    elemento_a_buscar = Enum.random(1..100)
    indice_elemento = buscar_elemento(elemento_a_buscar, lista)

    IO.puts "El elemento #{elemento_a_buscar} se encuentra en el índice #{indice_elemento}"

    lista_ordenada = ordenar_lista(lista)
    imprimir_lista(lista_ordenada)

    promedio = calcular_promedio(lista)
    IO.puts "El promedio de la lista es #{promedio}"
  end
end

MiModulo.main()
```

Explicación:

1. **Crear una lista:** La función `crear_lista/1` crea una lista de números aleatorios de tamaño `n`.
2. **Imprimir una lista:** La función `imprimir_lista/1` imprime los elementos de una lista.
3. **Buscar un elemento:** La función `buscar_elemento/2` busca un elemento en una lista y devuelve su índice si lo encuentra, o `nil` si no lo encuentra.
4. **Ordenar una lista:** La función `ordenar_lista/1` ordena una lista de números en orden ascendente.
5. **Calcular el promedio:** La función `calcular_promedio/1` calcula el promedio de los elementos de una lista.
6. **Función principal:** La función `main/0` crea una lista, imprime la lista, busca un elemento aleatorio en la lista, ordena la lista, calcula el promedio de la lista e imprime el resultado.