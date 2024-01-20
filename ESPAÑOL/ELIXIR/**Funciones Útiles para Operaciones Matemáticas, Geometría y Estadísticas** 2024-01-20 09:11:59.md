```elixir
# Crea una función de utilidad para convertir grados Celsius a Fahrenheit
def convert_celsius_to_fahrenheit(celsius) do
  # Utiliza la fórmula de conversión: (Celsius * 9/5) + 32
  ((celsius * 9) / 5) + 32
end

# Crea una función genérica para calcular el área de una forma geométrica
def calculate_area(shape, dimensions) do
  case shape do
    :rectangle -> dimensions[:width] * dimensions[:length]
    :circle -> :math.pi * dimensions[:radius] ** 2
    :triangle -> 0.5 * dimensions[:base] * dimensions[:height]
    # Agrega más formas si es necesario
  end
end

# Crea una función para generar una lista aleatoria de números enteros
def generate_random_integers(count, min, max) do
  for _ <- 1..count do
    :rand.uniform(min, max)
  end
end

# Crea una función para buscar un elemento en una lista usando búsqueda lineal
def linear_search(list, target) do
  for (index, element) in Enum.with_index(list) do
    if element == target do
      return index
    end
  end

  -1  # Elemento no encontrado
end

# Crea una función para ordenar una lista de números enteros en orden ascendente
def sort_integers_ascending(list) do
  Enum.sort(list, fn x, y -> x < y end)
end

# Crea una función para eliminar duplicados de una lista
def remove_duplicates(list) do
  Enum.uniq(list)
end

# Crea una función para encontrar el valor máximo en una lista
def find_max(list) do
  Enum.max(list)
end

# Crea una función para encontrar el valor mínimo en una lista
def find_min(list) do
  Enum.min(list)
end

# Crea una función para calcular el promedio de una lista de números enteros
def calculate_average(list) do
  Enum.sum(list) / Enum.count(list)
end

# Crea una función para calcular la mediana de una lista de números enteros
def calculate_median(list) do
  sorted_list = sort_integers_ascending(list)
  length = Enum.count(sorted_list)

  case length do
    0 -> 0
    1 -> sorted_list |> hd()
    even ->
      mid_index1 = (length / 2) - 1
      mid_index2 = mid_index1 + 1
      (sorted_list |> elem(mid_index1) + sorted_list |> elem(mid_index2)) / 2.0
    odd ->
      mid_index = (length / 2).floor()
      sorted_list |> elem(mid_index)
  end
end
```

Explicación del código:

1. **Función `convert_celsius_to_fahrenheit`**: Convierte grados Celsius a Fahrenheit utilizando la fórmula de conversión.

2. **Función `calculate_area`**: Calcula el área de una forma geométrica dada y sus dimensiones.

3. **Función `generate_random_integers`**: Genera una lista aleatoria de números enteros dentro de un rango especificado.

4. **Función `linear_search`**: Busca un elemento en una lista utilizando la búsqueda lineal.

5. **Función `sort_integers_ascending`**: Ordena una lista de números enteros en orden ascendente.

6. **Función `remove_duplicates`**: Elimina los elementos duplicados de una lista.

7. **Función `find_max`**: Encuentra el valor máximo en una lista.

8. **Función `find_min`**: Encuentra el valor mínimo en una lista.

9. **Función `calculate_average`**: Calcula el promedio de una lista de números enteros.

10. **Función `calculate_median`**: Calcula la mediana de una lista de números enteros.