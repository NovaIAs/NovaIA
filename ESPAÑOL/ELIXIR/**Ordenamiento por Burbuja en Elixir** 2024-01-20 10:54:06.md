```elixir
# Implementación del algoritmo de ordenamiento por burbuja en Elixir

# Función para ordenar una lista de números enteros de menor a mayor usando el algoritmo de ordenamiento por burbuja
defmodule OrdenamientoPorBurbuja do
  def ordenar(lista) do
    # Comprobar que la lista sea válida
    if is_list(lista) and Enum.all?(lista, &is_integer/1) do
      # Ordenar la lista usando el algoritmo de ordenamiento por burbuja
      ordenar_por_burbuja(lista)
    else
      # La lista no es válida
      {:error, "La lista debe ser una lista de números enteros"}
    end
  end

  # Función auxiliar para ordenar la lista usando el algoritmo de ordenamiento por burbuja
  defp ordenar_por_burbuja(lista) do
    # Base del caso: si la lista está vacía o tiene un solo elemento, está ordenada
    if lista == [] or lista == [_] do
      lista
    else
      # Dividir la lista en dos partes: el primer elemento y el resto de la lista
      [primero | resto] = lista

      # Ordenar recursivamente el resto de la lista
      resto_ordenado = ordenar_por_burbuja(resto)

      # Insertar el primer elemento en la posición correcta en el resto de la lista ordenada
      insertar_en_orden(primero, resto_ordenado)
    end
  end

  # Función auxiliar para insertar un elemento en la posición correcta en una lista ordenada
  defp insertar_en_orden(elemento, lista_ordenada) do
    # Caso base: si la lista ordenada está vacía, el elemento debe ser el primer elemento
    if lista_ordenada == [] do
      [elemento]
    else
      # Caso base: si el elemento es menor o igual que el primer elemento de la lista ordenada, insertarlo al inicio de la lista
      if elemento <= Enum.at(lista_ordenada, 0) do
        [elemento | lista_ordenada]
      else
        # Recursivamente insertar el elemento en la posición correcta en el resto de la lista ordenada
        [primero | resto] = lista_ordenada
        [primero | insertar_en_orden(elemento, resto)]
      end
    end
  end
end

# Usar la función para ordenar una lista de números enteros
lista_desordenada = [5, 3, 1, 2, 4]
{:ok, lista_ordenada} = OrdenamientoPorBurbuja.ordenar(lista_desordenada)
IO.inspect(lista_ordenada)
```

Explicación del código:

* La función `OrdenamientoPorBurbuja.ordenar/1` recibe una lista de números enteros y devuelve una lista ordenada de menor a mayor.
* La función comprueba primero si la lista es válida, es decir, si es una lista y todos sus elementos son números enteros.
* Si la lista es válida, la función utiliza la función auxiliar `ordenar_por_burbuja/1` para ordenarla.
* La función `ordenar_por_burbuja/1` utiliza el algoritmo de ordenamiento por burbuja para ordenar la lista.
* El algoritmo de ordenamiento por burbuja funciona de la siguiente manera:
    * Se recorre la lista de principio a fin.
    * Durante cada recorrido, se comparan los elementos adyacentes.
    * Si un elemento es mayor que el siguiente, se intercambian.
    * El proceso se repite hasta que no se hagan más intercambios.
* La función `insertar_en_orden/2` se utiliza para insertar un elemento en la posición correcta en una lista ordenada.
* La función funciona de la siguiente manera:
    * Se comprueba si la lista ordenada está vacía.
    * Si está vacía, el elemento se inserta al inicio de la lista.
    * Si no está vacía, se compara el elemento con el primer elemento de la lista ordenada.
    * Si el elemento es menor o igual que el primer elemento, se inserta al inicio de la lista.
    * Si el elemento es mayor que el primer elemento, se inserta recursivamente en el resto de la lista ordenada.
* Para utilizar la función, se crea una lista de números enteros desordenada y se llama a la función `OrdenamientoPorBurbuja.ordenar/1` para ordenarla.
* El resultado de la función se imprime en la consola.