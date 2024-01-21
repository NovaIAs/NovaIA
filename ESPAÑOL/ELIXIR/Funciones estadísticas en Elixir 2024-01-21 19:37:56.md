```elixir
# Definimos una función que recibe una lista de números y devuelve la suma de los mismos.
def suma_lista(lista) do
  Enum.reduce(lista, 0, fn x, acc -> acc + x end)
end

# Definimos una función que recibe una lista de números y devuelve el producto de los mismos.
def producto_lista(lista) do
  Enum.reduce(lista, 1, fn x, acc -> acc * x end)
end

# Definimos una función que recibe una lista de números y devuelve el máximo de los mismos.
def max_lista(lista) do
  Enum.max(lista)
end

# Definimos una función que recibe una lista de números y devuelve el mínimo de los mismos.
def min_lista(lista) do
  Enum.min(lista)
end

# Definimos una función que recibe una lista de números y devuelve la media de los mismos.
def media_lista(lista) do
  suma = suma_lista(lista)
  longitud = Enum.count(lista)
  suma / longitud
end

# Definimos una función que recibe una lista de números y devuelve la desviación estándar de los mismos.
def desviacion_estándar_lista(lista) do
  media = media_lista(lista)
  varianza = Enum.reduce(lista, 0, fn x, acc -> acc + ((x - media) ** 2) end) / (Enum.count(lista) - 1)
  :math.sqrt(varianza)
end

# Definimos una función que recibe una lista de números y devuelve la mediana de los mismos.
def mediana_lista(lista) do
  lista_ordenada = Enum.sort(lista)
  longitud = Enum.count(lista_ordenada)
  if rem(longitud, 2) == 0 do
    (lista_ordenada[longitud div 2] + lista_ordenada[longitud div 2 - 1]) / 2
  else
    lista_ordenada[longitud div 2]
  end
end

# Definimos una función que recibe una lista de números y devuelve el modo de los mismos.
def modo_lista(lista) do
  valores_unicos = Enum.uniq(lista)
  frecuencias = Enum.map(valores_unicos, fn x -> Enum.count(lista, x) end)
  max_frecuencia = Enum.max(frecuencias)
  valores_unicos[Enum.find_index(frecuencias, &(&1 == max_frecuencia))]
end

# Definimos una función que recibe una lista de números y devuelve la correlación entre ellos.
def correlacion_lista(lista1, lista2) do
  n = Enum.count(lista1)
  media1 = media_lista(lista1)
  media2 = media_lista(lista2)
  covarianza = Enum.reduce(Enum.zip(lista1, lista2), 0, fn {x1, x2}, acc -> acc + ((x1 - media1) * (x2 - media2)) end) / (n - 1)
  desviacion_estándar1 = desviacion_estándar_lista(lista1)
  desviacion_estándar2 = desviacion_estándar_lista(lista2)
  covarianza / (desviacion_estándar1 * desviacion_estándar2)
end

# Definimos una función que recibe una lista de números y devuelve la regresión lineal de los mismos.
def regresion_lineal_lista(lista1, lista2) do
  n = Enum.count(lista1)
  media1 = media_lista(lista1)
  media2 = media_lista(lista2)
  covarianza = Enum.reduce(Enum.zip(lista1, lista2), 0, fn {x1, x2}, acc -> acc + ((x1 - media1) * (x2 - media2)) end) / (n - 1)
  desviacion_estándar