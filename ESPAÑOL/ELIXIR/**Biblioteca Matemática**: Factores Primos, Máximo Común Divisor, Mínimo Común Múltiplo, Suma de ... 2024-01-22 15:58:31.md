```elixir
# Módulo que define una función para calcular el factorial de un número.

defmodule Factorial do
  # Función que calcula el factorial de un número.

  def factorial(n) when n >= 0 do
    # Si el número es 0, el factorial es 1.

    if n == 0, do: 1, else:

    # Si el número es mayor que 0, el factorial es el número multiplicado
    # por el factorial del número anterior.

    n * factorial(n - 1)
  end
end

# Módulo que define una función para calcular el máximo común divisor de dos números.

defmodule MaximoComunDivisor do
  # Función que calcula el máximo común divisor de dos números.

  def maximo_comun_divisor(a, b) do
    # Si b es 0, el máximo común divisor es a.

    if b == 0, do: a, else:

    # Si b no es 0, el máximo común divisor es el máximo común divisor de
    # b y el resto de dividir a entre b.

    maximo_comun_divisor(b, rem(a, b))
  end
end

# Módulo que define una función para calcular el mínimo común múltiplo de dos números.

defmodule MinimoComunMultiplo do
  # Función que calcula el mínimo común múltiplo de dos números.

  def minimo_comun_multiplo(a, b) do
    # El mínimo común múltiplo es el producto de los dos números dividido por
    # el máximo común divisor.

    a * b / MaximoComunDivisor.maximo_comun_divisor(a, b)
  end
end

# Módulo que define una función para calcular la suma de dos números complejos.

defmodule SumaComplejos do
  # Función que calcula la suma de dos números complejos.

  def suma_complejos({real1, imaginario1}, {real2, imaginario2}) do
    # La suma de dos números complejos es la suma de sus partes reales e
    # imaginarias, respectivamente.

    {real1 + real2, imaginario1 + imaginario2}
  end
end

# Módulo que define una función para calcular el producto de dos números complejos.

defmodule ProductoComplejos do
  # Función que calcula el producto de dos números complejos.

  def producto_complejos({real1, imaginario1}, {real2, imaginario2