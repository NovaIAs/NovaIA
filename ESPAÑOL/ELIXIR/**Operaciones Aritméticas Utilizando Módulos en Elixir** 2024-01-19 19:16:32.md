```elixir
# Definición de Módulos
defmodule Suma do
  def calcular(a, b) do
    a + b
  end
end

defmodule Resta do
  def calcular(a, b) do
    a - b
  end
end

defmodule Multiplicación do
  def calcular(a, b) do
    a * b
  end
end

defmodule División do
  def calcular(a, b) do
    a / b
  end
end

# Definición de la Función Principal
defmodule Main do
  def inicio() do
    # Creación de expresiones aritméticas
    expresión_1 = Suma.calcular(2, 3)
    expresión_2 = Resta.calcular(5, 2)
    expresión_3 = Multiplicación.calcular(3, 4)
    expresión_4 = División.calcular(12, 3)

    # Impresión de los resultados
    IO.puts("Suma: #{expresión_1}")
    IO.puts("Resta: #{expresión_2}")
    IO.puts("Multiplicación: #{expresión_3}")
    IO.puts("División: #{expresión_4}")
  end
end

# Llamada a la Función Principal
Main.inicio()
```

Explicación del código:


* Definición de Módulos: En elixir, los módulos se utilizan para organizar y agrupar funciones relacionadas. En este código, se definen cuatro módulos: `Suma`, `Resta`, `Multiplicación` y `División`. Cada módulo contiene una función que realiza la operación correspondiente.


* Función Principal `inicio()`: La función `inicio()` es la función principal del programa y es responsable de crear las expresiones aritméticas y mostrarlas en la pantalla.


* Creación de Expresiones Aritméticas: Se crean cuatro expresiones aritméticas utilizando las funciones definidas en los módulos. Estas expresiones son:

  * `expresión_1 = Suma.calcular(2, 3)`: Suma de 2 y 3.
  * `expresión_2 = Resta.calcular(5, 2)`: Resta de 5 y 2.
  * `expresión_3 = Multiplicación.calcular(3, 4)`: Multiplicación de 3 y 4.
  * `expresión_4 = División.calcular(12, 3)`: División de 12 entre 3.


* Impresión de los Resultados: Los resultados de las expresiones aritméticas se imprimen en la consola utilizando la función `IO.puts()`.


* Llamada a la Función Principal: La función `inicio()` es llamada desde el ámbito global para ejecutar el programa.