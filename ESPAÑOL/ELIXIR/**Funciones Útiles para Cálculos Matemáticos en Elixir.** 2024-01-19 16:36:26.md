```elixir
# Definir función factorial
defmodule Factorial do
  import Ecto.Query

  def fact(n) when n == 0, do: 1
  def fact(n), do: n * fact(n-1)
end

# Definir función de suma
defmodule Suma do
  import Ecto.Query

  def sum(list) do
    Enum.reduce(list, 0, fn x, acc -> x + acc end)
  end
end

# Definir función de búsqueda en lista
defmodule Busqueda do
  import Ecto.Query

  def find(list, elemento) do
    Enum.find_index(list, fn x -> x == elemento end)
  end
end

# Definir función de ordenamiento
defmodule Ordenamiento do
  import Ecto.Query

  def sort(list) do
    list |> Enum.sort()
  end
end

# Definir función de inversión de cadena
defmodule InversionCadena do
  import Ecto.Query

  def reverse(cadena) do
    cadena |> String.reverse()
  end
end

# Definir función de cálculo de Fibonacci
defmodule Fibonacci do
  import Ecto.Query

  def fib(n) when n == 0, do: 0
  def fib(n) when n == 1, do: 1
  def fib(n), do: fib(n-1) + fib(n-2)
end

# Definir función de cálculo de área de un círculo
defmodule AreaCirculo do
  import Ecto.Query

  def area(radio) do
    :math.pi * radio**2
  end
end

# Definir función de cálculo de volumen de un cubo
defmodule VolumenCubo do
  import Ecto.Query

  def volume(lado) do
    lado**3
  end
end

# Definir función de cálculo de distancia euclidiana
defmodule DistanciaEuclidiana do
  import Ecto.Query

  def distance(point1, point2) do
    :math.sqrt(
      Enum.zip_reduce(point1, point2, 0, fn {x1, y1}, {x2, y2}, acc ->
        acc + (:math.pow(x1 - x2, 2) + :math.pow(y1 - y2, 2))
      end)
    )
  end
end
```