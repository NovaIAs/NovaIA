```elixir
# Definir una función para generar números aleatorios
def generar_numeros_aleatorios(cantidad) do
  Enum.take(Enum.map(1..cantidad, fn _ -> :rand.uniform(100) end), cantidad)
end

# Definir una función para calcular la media de una lista de números
def calcular_media(lista) do
  Enum.sum(lista) / Enum.count(lista)
end

# Definir una función para calcular la desviación estándar de una lista de números
def calcular_desviacion_estandar(lista) do
  varianza = Enum.reduce(lista, 0, fn x, acc -> acc + ((x - calcular_media(lista)) ** 2) end) / (Enum.count(lista) - 1)
  :math.sqrt(varianza)
end

# Definir una función para generar una gráfica de barras
def generar_grafica_barras(lista, titulo) do
  IO.puts(titulo)
  Enum.each(lista, fn x -> IO.write("*" * x) end)
end

# Generar una lista de 100 números aleatorios
numeros_aleatorios = generar_numeros_aleatorios(100)

# Calcular la media de la lista de números aleatorios
media = calcular_media(numeros_aleatorios)

# Calcular la desviación estándar de la lista de números aleatorios
desviacion_estandar = calcular_desviacion_estandar(numeros_aleatorios)

# Generar una gráfica de barras de la lista de números aleatorios
generar_grafica_barras(numeros_aleatorios, "Gráfica de barras de los números aleatorios")

# Imprimir la media y la desviación estándar de la lista de números aleatorios
IO.puts("Media: #{media}")
IO.puts("Desviación estándar: #{desviacion_estandar}")
```

Este código genera una lista de 100 números aleatorios, calcula la media y la desviación estándar de la lista, y genera una gráfica de barras de la lista.

La función `generar_numeros_aleatorios` utiliza la función `:rand.uniform` para generar una lista de números aleatorios. La función `calcular_media` utiliza la función `Enum.sum` para sumar los números de la lista y la función `Enum.count` para contar el número de elementos de la lista. La función `calcular_desviacion_estandar` utiliza la función `Enum.reduce` para calcular la varianza de la lista y la función `:math.sqrt` para calcular la desviación estándar. La función `generar_grafica_barras` utiliza la función `IO.puts` para imprimir el título de la gráfica y la función `Enum.each` para imprimir una barra de asteriscos para cada número de la lista.