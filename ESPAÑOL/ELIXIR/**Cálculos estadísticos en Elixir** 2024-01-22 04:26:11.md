```elixir
# Definimos un módulo para agrupar las funciones
defmodule Calculos {

    # Función para calcular el factorial de un número
    def factorial(n) do
        if n == 0, do: 1, else: n * factorial(n-1)
    end

    # Función para calcular la media de una lista de números
    def media(lista) do
        suma = Enum.reduce(lista, 0, &(&1 + &2))
        largo = Enum.count(lista)
        suma / largo
    end

    # Función para calcular la desviación estándar de una lista de números
    def desviacion_estandar(lista) do
        promedio = media(lista)
        varianza = Enum.reduce(lista, 0, &(&1 + (&2 - promedio) ^ 2)) / (Enum.count(lista) - 1)
        Math.sqrt(varianza)
    end

    # Función para calcular la correlación entre dos listas de números
    def correlacion(lista_x, lista_y) do
        promedio_x = media(lista_x)
        promedio_y = media(lista_y)
        suma_xy = Enum.zip(lista_x, lista_y) |> Enum.reduce(0, &(&1 + &2.1 * &2.2))
        suma_x_cuadrada = Enum.reduce(lista_x, 0, &(&1 + &2 ^ 2))
        suma_y_cuadrada = Enum.reduce(lista_y, 0, &(&1 + &2 ^ 2))
        (suma_xy - promedio_x * promedio_y * Enum.count(lista_x)) / (Math.sqrt(suma_x_cuadrada - promedio_x ^ 2 * Enum.count(lista_x)) * Math.sqrt(suma_y_cuadrada - promedio_y ^ 2 * Enum.count(lista_y)))
    end

}

# Ejemplo de uso de las funciones
lista_numeros = [1, 2, 3, 4, 5]

IO.puts("Factorial de 5: #{Calculos.factorial(5)}")
IO.puts("Media de la lista: #{Calculos.media(lista_numeros)}")
IO.puts("Desviación estándar de la lista: #{Calculos.desviacion_estandar(lista_numeros)}")

lista_x = [1, 2, 3, 4, 5]
lista_y = [2, 4, 6, 8, 10]

IO.puts("Correlación entre las listas: #{Calculos.correlacion(lista_x, lista_y)}")
```

Explicación del código:

* Definimos un módulo `Calculos` para agrupar las funciones.


* Definimos una función `factorial` para calcular el factorial de un número.


* Definimos una función `media` para calcular la media de una lista de números.


* Definimos una función `desviacion_estandar` para calcular la desviación estándar de una lista de números.


* Definimos una función `correlacion` para calcular la correlación entre dos listas de números.


* Creamos una lista de números de ejemplo.


* Llamamos a las funciones con la lista de ejemplo y mostramos los resultados en la consola.