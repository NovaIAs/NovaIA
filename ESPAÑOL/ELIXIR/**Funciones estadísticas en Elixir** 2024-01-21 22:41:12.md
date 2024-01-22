```elixir
# Definimos una función `suma_cuadrados` que toma una lista de números y devuelve la suma de sus cuadrados.
def suma_cuadrados(lista) do
  Enum.reduce(lista, 0, fn x, acc -> acc + x * x end)
end

# Definimos una función `media` que toma una lista de números y devuelve su media.
def media(lista) do
  suma = suma_cuadrados(lista)
  longitud = length(lista)
  suma / longitud
end

# Definimos una función `mediana` que toma una lista de números y devuelve su mediana.
def mediana(lista) do
  lista_ordenada = Enum.sort(lista)
  longitud = length(lista_ordenada)

  case longitud do
    1 -> lista_ordenada |> hd()
    par -> (lista_ordenada |> tl() |> hd() + lista_ordenada |> Enum.at(longitud - 1)) / 2
    impar -> lista_ordenada |> Enum.at(div(longitud - 1, 2))
  end
end

# Definimos una función `desviación_estándar` que toma una lista de números y devuelve su desviación estándar.
def desviación_estándar(lista) do
  media_lista = media(lista)
  varianza = Enum.reduce(lista, 0, fn x, acc -> acc + (x - media_lista) * (x - media_lista) end) / (length(lista) - 1)
  :math.sqrt(varianza)
end

# Definimos una función `coeficiente_de_variación` que toma una lista de números y devuelve su coeficiente de variación.
def coeficiente_de_variación(lista) do
  media_lista = media(lista)
  desviación_estándar_lista = desviación_estándar(lista)
  desviación_estándar_lista / media_lista
end

# Definimos una función `percentil` que toma una lista de números, un valor de percentil y devuelve el valor del percentil.
def percentil(lista, percentil) do
  lista_ordenada = Enum.sort(lista)
  longitud = length(lista_ordenada)
  posición = percentil * longitud / 100

  case posición do
    1 -> lista_ordenada |> hd()
    longitud -> lista_ordenada |> Enum.at(longitud - 1)
    _ -> lista_ordenada |> Enum.at(trunc(posición) - 1)
  end
end

# Definimos una función `intervalo_de_confianza` que toma una lista de números, un nivel de confianza y devuelve el intervalo de confianza al nivel de confianza especificado.
def intervalo_de_confianza(lista, nivel_de_confianza) do
  media_lista = media(lista)
  desviación_estándar_lista = desviación_estándar(lista)
  longitud = length(lista)

  z = caso nivel_de_confianza do
    90 -> 1.645
    95 -> 1.960
    99 -> 2.576
  end

  media_lista - z * desviación_estándar_lista / :math.sqrt(longitud),
  media_lista + z * desviación_estándar_lista / :math.sqrt(longitud)
end

# Definimos una lista de números para probar las funciones.
lista = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

# Imprimimos los resultados de las funciones.
IO.puts("Suma de cuadrados: #{suma_cuadrados(lista)}")
IO.puts("Media: #{media(lista)}")
IO.puts("Mediana: #{mediana(lista)}")
IO.puts("Desviación estándar: #{desviación_estándar(lista)}")
IO.puts("Coeficiente de variación: #{coeficiente_de_variación(lista)}")
IO.puts("Percentil 25: #{percentil(lista, 25)}")
IO.puts("Percentil 50: #{percentil(lista, 50)}")
IO.puts("Percentil 75: #{percentil(lista, 75)}")
IO.puts("Intervalo de confianza al 95%: #{intervalo_de_confianza(lista, 95)}")
```

Este código define varias funciones estadísticas en Elixir. Las funciones son:

* `suma_cuadrados`: calcula la suma de los cuadrados de los números en una lista.
* `media`: calcula la media de los números en una lista.
* `mediana`: calcula la mediana de los números en una lista.
* `desviación_estándar`: calcula la desviación estándar de los números en una lista.
* `coeficiente_de_variación`: calcula el coeficiente de variación de los números en una lista.
* `percentil`: calcula el percentil especificado de los números en una lista.
* `intervalo_de_confianza`: calcula el intervalo de confianza al nivel de confianza especificado de los números en una lista.

El código también incluye una lista de números para probar las funciones y luego imprime los resultados de las funciones.