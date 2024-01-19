```elixir
# Definiendo un módulo para organizar el código
defmodule HolaMundo do

  # Definiendo una función para saludar
  def saludar(nombre) do
    # Utilizando string interpolation para construir el mensaje de saludo
    "¡Hola, #{nombre}! Bienvenido al mundo de Elixir."
  end

  # Definiendo una función para sumar dos números
  def sumar(a, b) do
    # Utilizando el operador "+" para sumar los dos números
    a + b
  end

  # Definiendo una función para restar dos números
  def restar(a, b) do
    # Utilizando el operador "-" para restar los dos números
    a - b
  end

  # Definiendo una función para multiplicar dos números
  def multiplicar(a, b) do
    # Utilizando el operador "*" para multiplicar los dos números
    a * b
  end

  # Definiendo una función para dividir dos números
  def dividir(a, b) do
    # Utilizando el operador "/" para dividir los dos números
    a / b
  end

  # Definiendo una función para calcular el promedio de una lista de números
  def promedio(lista) do
    # Utilizando la función "Enum.sum/1" para sumar todos los elementos de la lista
    suma = Enum.sum(lista)

    # Utilizando la función "Enum.count/1" para contar el número de elementos de la lista
    numero_elementos = Enum.count(lista)

    # Calculando el promedio dividiendo la suma por el número de elementos
    suma / numero_elementos
  end

  # Definiendo una función para filtrar una lista de números según un criterio
  def filtrar(lista, criterio) do
    # Utilizando la función "Enum.filter/2" para filtrar la lista según el criterio dado
    Enum.filter(lista, criterio)
  end

  # Definiendo una función para ordenar una lista de números
  def ordenar(lista) do
    # Utilizando la función "Enum.sort/1" para ordenar la lista
    Enum.sort(lista)
  end

  # Definiendo una función para invertir una lista
  def invertir(lista) do
    # Utilizando la función "Enum.reverse/1" para invertir la lista
    Enum.reverse(lista)
  end

  # Definiendo una función para concatenar dos listas
  def concatenar(lista1, lista2) do
    # Utilizando la función "Enum.concat/2" para concatenar las dos listas
    Enum.concat(lista1, lista2)
  end

  # Definiendo una función para dividir una lista en sublistas
  def dividir_en_sublistas(lista, tamaño_sublista) do
    # Utilizando la función "Enum.chunk/2" para dividir la lista en sublistas del tamaño especificado
    Enum.chunk(lista, tamaño_sublista)
  end

  # Definiendo una función para generar una lista de números desde un rango
  def generar_lista_numeros(inicio, fin) do
    # Utilizando la función "Enum.range/2" para generar una lista de números desde el inicio hasta el fin
    Enum.range(inicio, fin)
  end

  # Definiendo una función para crear un mapa a partir de una lista de pares clave-valor
  def crear_mapa(lista_pares) do
    # Utilizando la función "Map.new/1" para crear un mapa a partir de la lista de pares
    Map.new(lista_pares)
  end

  # Definiendo una función para obtener el valor de una clave en un mapa
  def obtener_valor_mapa(mapa, clave) do
    # Utilizando la función "Map.get/2" para obtener el valor de la clave especificada en el mapa
    Map.get(mapa, clave)
  end

  # Definiendo una función para añadir un par clave-valor a un mapa
  def añadir_par_mapa(mapa, clave, valor) do
    # Utilizando la función "Map.put/3" para añadir un par clave-valor al mapa
    Map.put(mapa, clave, valor)
  end

  # Definiendo una función para eliminar un par clave-valor de un mapa
  def eliminar_par_mapa(mapa, clave) do
    # Utilizando la función "Map.delete/2" para eliminar un par clave-valor del mapa
    Map.delete(mapa, clave)
  end

  # Definiendo una función para iterar sobre un mapa
  def iterar_mapa(mapa) do
    # Utilizando la función "Enum.each/2" para iterar sobre el mapa
    Enum.each(mapa, fn({clave, valor}) ->
      # Imprimiendo la clave y el valor del par actual
      IO.puts("#{clave}: #{valor}")
    end)
  end

  # Definiendo una función para crear un proceso
  def crear_proceso(funcion) do
    # Utilizando la función "spawn/1" para crear un proceso que ejecuta la función especificada
    spawn(funcion)
  end

  # Definiendo una función para enviar un mensaje a un proceso
  def enviar_mensaje(proceso, mensaje) do
    # Utilizando la función "send/2" para enviar un mensaje al proceso especificado
    send(proceso, mensaje)
  end

  # Definiendo una función para recibir un mensaje de un proceso
  def recibir_mensaje() do
    # Utilizando la función "receive/1" para recibir un mensaje de cualquier proceso
    receive do
      mensaje ->
        # Imprimiendo el mensaje recibido
        IO.puts("Mensaje recibido: #{mensaje}")
    end
  end
end

# Utilizando el módulo HolaMundo para saludar al usuario
nombre_usuario = "Juan"
IO.puts(HolaMundo.saludar(nombre_usuario))

# Utilizando el módulo HolaMundo para sumar dos números
numero1 = 5
numero2 = 10
IO.puts("La suma de #{numero1} y #{numero2} es: #{HolaMundo.sumar(numero1, numero2)}")

# Utilizando el módulo HolaMundo para restar dos números
numero1 = 15
numero2 = 5
IO.puts("La resta de #{numero1} y #{numero2} es: #{HolaMundo.restar(numero1, numero2)}")

# Utilizando el módulo HolaMundo para multiplicar dos números
numero1 = 3
numero2 = 4
IO.puts("La multiplicación de #{numero1} y #{numero2} es: #{HolaMundo.multiplicar(numero1, numero2)}")

# Utilizando el módulo HolaMundo para dividir dos números
numero1 = 12
numero2 = 3
IO.puts("La división de #{numero1} y #{numero2} es: #{HolaMundo.dividir(numero1, numero2)}")

# Utilizando el módulo HolaMundo para calcular el promedio de una lista de números
lista_numeros = [1, 2, 3, 4, 5]
IO.puts("El promedio de la lista [1, 2, 3, 4, 5] es: #{HolaMundo.promedio(lista_numeros)}")

# Utilizando el módulo HolaMundo para filtrar una lista de números según un criterio
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
criterio = fn(numero) -> numero % 2 == 0 end
IO.puts("La lista filtrada de [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] según el criterio de números pares es: #{HolaMundo.filtrar(lista_numeros, criterio)}")

# Utilizando el módulo HolaMundo para ordenar una lista de números
lista_numeros = [5, 2, 8, 3, 1, 4, 6, 7, 9]
IO.puts("La lista [5, 2, 8, 3, 1, 4, 6, 7, 9] ordenada es: #{HolaMundo.ordenar(lista_numeros)}")

# Utilizando el módulo HolaMundo para invertir una lista
lista_numeros = [1, 2, 3, 4, 5]
IO.puts("La lista [1, 2, 3, 4, 5] invertida es: #{HolaMundo.invertir(lista_numeros)}")

# Utilizando el módulo HolaMundo para concatenar dos listas
lista1 = [1, 2, 3]
lista2 = [4, 5, 6]
IO.puts("La concatenación de [1, 2, 3] y [4, 5, 6] es: #{HolaMundo.concatenar(lista1, lista2)}")

# Utilizando el módulo HolaMundo para dividir una lista en sublistas
lista_numeros = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]
tamaño_sublista = 3
IO.puts("La lista [1, 2, 3, 4, 5, 6, 7, 8, 9, 10] dividida en sublistas de tamaño 3 es: #{HolaMundo.dividir_en_sublistas(lista_numeros, tamaño_sublista)}")

# Utilizando el módulo HolaMundo para generar una lista de números desde un rango
inicio = 1
fin = 10
IO.puts("La lista de números desde #{inicio} hasta #{fin} es: #{HolaMundo.generar_lista_numeros(inicio, fin)}")

# Utilizando el módulo HolaMundo para crear un mapa a partir de una lista de pares clave-valor
lista_pares = [{"nombre": "Juan"}, {"edad": 25}, {"profesión": "Ingeniero"}]
IO.puts("El mapa creado a partir de la lista [{\"nombre\": \"Juan\"}, {\"edad\": 25}, {\"profesión\": \"Ingeniero\"}] es: #{HolaMundo.crear_mapa(lista_pares)}")

# Utilizando el módulo HolaMundo para obtener el valor de una clave en un mapa
mapa = %{"nombre": "Juan", "edad": 25, "profesión": "Ingeniero"}
clave = "nombre"
IO.puts("El valor de la clave \"nombre\" en el mapa es: #{HolaMundo.obtener_valor_mapa(mapa