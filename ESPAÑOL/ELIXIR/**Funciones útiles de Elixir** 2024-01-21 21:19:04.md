```elixir
# Crear una función que reciba una lista de números y devuelva la suma de los pares

defmodule SumaPares do
  def suma_pares(lista) do
    Enum.reduce(lista, 0, fn x, acc ->
      if rem(x, 2) == 0, do: x + acc, else: acc
    end)
  end
end

# Crear una función que reciba dos números y devuelva el máximo común divisor

defmodule MaximoComunDivisor do
  def mcd(a, b) when a == b, do: a
  def mcd(a, b) when a > b, do: mcd(b, rem(a, b))
  def mcd(a, b) when a < b, do: mcd(a, rem(b, a))
end

# Crear una función que reciba una cadena de texto y devuelva el número de palabras

defmodule NumeroPalabras do
  def numero_palabras(texto) do
    String.split(texto, ~r/\s+/) |> Enum.count
  end
end

# Crear una función que reciba una lista de cadenas de texto y devuelva la cadena de texto más larga

defmodule CadenaMasLarga do
  def cadena_mas_larga(lista) do
    Enum.max_by(lista, &String.length/1)
  end
end

# Crear una función que reciba un mapa y devuelva una lista de las claves

defmodule ClavesMapa do
  def claves_mapa(mapa) do
    Map.keys(mapa)
  end
end

# Crear una función que reciba un mapa y devuelva una lista de los valores

defmodule ValoresMapa do
  def valores_mapa(mapa) do
    Map.values(mapa)
  end
end

# Crear una función que reciba una lista de tuplas y devuelva un mapa

defmodule TuplasAMapa do
  def tuplas_a_mapa(lista) do
    Enum.reduce(lista, %{}, fn {clave, valor}, acc ->
      Map.put(acc, clave, valor)
    end)
  end
end

# Crear una función que reciba un mapa y devuelva una lista de tuplas

defmodule MapaATuplas do
  def mapa_a_tuplas(mapa) do
    Enum.map(mapa, fn {clave, valor} -> {clave, valor} end)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista de los números que son mayores que 10

defmodule MayoresQue10 do
  def mayores_que_10(lista) do
    Enum.filter(lista, fn x -> x > 10 end)
  end
end

# Crear una función que reciba una lista de cadenas de texto y devuelva una lista de las cadenas de texto que empiezan por "A"

defmodule EmpiezanPorA do
  def empiezan_por_a(lista) do
    Enum.filter(lista, fn x -> String.starts_with?(x, "A") end)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista de los números que son pares y mayores que 10

defmodule ParesMayoresQue10 do
  def pares_mayores_que_10(lista) do
    Enum.filter(lista, fn x -> rem(x, 2) == 0 and x > 10 end)
  end
end
```

Explicación del código:

* La función `SumaPares` recibe una lista de números y devuelve la suma de los pares.
* La función `MaximoComunDivisor` recibe dos números y devuelve el máximo común divisor.
* La función `NumeroPalabras` recibe una cadena de texto y devuelve el número de palabras.
* La función `CadenaMasLarga` recibe una lista de cadenas de texto y devuelve la cadena de texto más larga.
* La función `ClavesMapa` recibe un mapa y devuelve una lista de las claves.
* La función `ValoresMapa` recibe un mapa y devuelve una lista de los valores.
* La función `TuplasAMapa` recibe una lista de tuplas y devuelve un mapa.
* La función `MapaATuplas` recibe un mapa y devuelve una lista de tuplas.
* La función `MayoresQue10` recibe una lista de números y devuelve una lista de los números que son mayores que 10.
* La función `EmpiezanPorA` recibe una lista de cadenas de texto y devuelve una lista de las cadenas de texto que empiezan por "A".
* La función `ParesMayoresQue10` recibe una lista de números y devuelve una lista de los números que son pares y mayores que 10.