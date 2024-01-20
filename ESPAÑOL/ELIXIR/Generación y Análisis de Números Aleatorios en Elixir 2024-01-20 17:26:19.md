```elixir
# Función para generar un número aleatorio entre dos límites
def generar_aleatorio(min, max) do
  # Se utiliza la función `rand/1` del módulo `Kernel` para generar un número aleatorio
  Kernel.rand(max - min) + min
end

# Función para generar una lista de números aleatorios entre dos límites
def generar_lista_aleatorio(n, min, max) do
  # Se utiliza la función `Enum.map/3` del módulo `Enum` para generar una lista de números aleatorios.
  # La función `Enum.map/3` toma una lista como primer argumento, una función como segundo argumento y una lista como tercer argumento.
  # La función `Enum.map/3` aplica la función al primer elemento de la lista, al segundo elemento de la lista, y así sucesivamente, y devuelve una nueva lista.
  # En este caso, la función `Enum.map/3` aplica la función `generar_aleatorio/2` al número `n`, al valor mínimo `min` y al valor máximo `max`.
  Enum.map(1..n, fn(_) -> generar_aleatorio(min, max) end)
end

# Función para calcular la media de una lista de números
def calcular_media(lista) do
  # Se utiliza la función `Enum.sum/1` del módulo `Enum` para calcular la suma de los elementos de la lista.
  # La función `Enum.sum/1` toma una lista como argumento y devuelve la suma de los elementos de la lista.
  suma = Enum.sum(lista)

  # Se divide la suma de los elementos de la lista por el número de elementos de la lista para obtener la media.
  suma / Enum.count(lista)
end

# Función para calcular la desviación estándar de una lista de números
def calcular_desviacion_estandar(lista) do
  # Se utiliza la función `Enum.mean/1` del módulo `Enum` para calcular la media de los elementos de la lista.
  # La función `Enum.mean/1` toma una lista como argumento y devuelve la media de los elementos de la lista.
  media = Enum.mean(lista)

  # Se utiliza la función `Enum.map/3` del módulo `Enum` para calcular la diferencia entre cada elemento de la lista y la media.
  # La función `Enum.map/3` toma una lista como primer argumento, una función como segundo argumento y una lista como tercer argumento.
  # La función `Enum.map/3` aplica la función al primer elemento de la lista, al segundo elemento de la lista, y así sucesivamente, y devuelve una nueva lista.
  # En este caso, la función `Enum.map/3` aplica la función `fn(x) -> x - media end` a cada elemento de la lista `lista`.
  diferencias = Enum.map(lista, fn(x) -> x - media end)

  # Se utiliza la función `Enum.sum/1` del módulo `Enum` para calcular la suma de los cuadrados de las diferencias.
  # La función `Enum.sum/1` toma una lista como argumento y devuelve la suma de los elementos de la lista.
  suma_cuadrados_diferencias = Enum.sum(Enum.map(diferencias, fn(x) -> x * x end))

  # Se divide la suma de los cuadrados de las diferencias por el número de elementos de la lista menos uno para obtener la varianza.
  varianza = suma_cuadrados_diferencias / (Enum.count(lista) - 1)

  # Se calcula la raíz cuadrada de la varianza para obtener la desviación estándar.
  :math.sqrt(varianza)
end

# Se genera una lista de 1000 números aleatorios entre 0 y 1000
lista = generar_lista_aleatorio(1000, 0, 1000)

# Se calcula la media de la lista
media = calcular_media(lista)

# Se calcula la desviación estándar de la lista
desviacion_estandar = calcular_desviacion_estandar(lista)

# Se imprime la media y la desviación estándar
IO.puts("Media: #{media}")
IO.puts("Desviación estándar: #{desviacion_estandar}")
```

Explicación:

* La función `generar_aleatorio/2` genera un número aleatorio entre dos límites.
* La función `generar_lista_aleatorio/3` genera una lista de números aleatorios entre dos límites.
* La función `calcular_media/1` calcula la media de una lista de números.
* La función `calcular_desviacion_estandar/1` calcula la desviación estándar de una lista de números.
* El código genera una lista de 1000 números aleatorios entre 0 y 1000.
* El código calcula la media y la desviación estándar de la lista.
* El código imprime la media y la desviación estándar.