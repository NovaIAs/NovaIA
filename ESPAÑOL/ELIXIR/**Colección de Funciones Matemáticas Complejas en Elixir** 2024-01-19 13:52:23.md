```elixir
# Crear una función que reciba dos números y devuelva su suma
defmodule Suma do
  def suma(a, b) do
    a + b
  end
end

# Crear una función que reciba una lista de números y devuelva su suma
defmodule SumaLista do
  def suma_lista(lista) do
    Enum.reduce(lista, 0, &Suma.suma(&1, &2))
  end
end

# Crear una función que reciba un número y devuelva su factorial
defmodule Factorial do
  def factorial(n) when n == 0, do: 1
  def factorial(n) do
    n * Factorial.factorial(n - 1)
  end
end

# Crear una función que reciba una lista de números y devuelva el número mayor
defmodule NumeroMayor do
  def numero_mayor(lista) do
    Enum.max(lista)
  end
end

# Crear una función que reciba una lista de números y devuelva el número menor
defmodule NumeroMenor do
  def numero_menor(lista) do
    Enum.min(lista)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista con los números pares
defmodule NumerosPares do
  def numeros_pares(lista) do
    Enum.filter(lista, &rem(&1, 2) == 0)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista con los números impares
defmodule NumerosImpares do
  def numeros_impares(lista) do
    Enum.filter(lista, &rem(&1, 2) == 1)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista con los números primos
defmodule NumerosPrimos do
  def numeros_primos(lista) do
    Enum.filter(lista, &es_primo(&1))
  end

  defp es_primo(n) when n <= 1, do: false
  defp es_primo(n) do
    Enum.all?(2..n-1, &rem(n, &1) != 0)
  end
end

# Crear una función que reciba una lista de números y devuelva una lista con los números perfectos
defmodule NumerosPerfectos do
  def numeros_perfectos(lista) do
    Enum.filter(lista, &es_perfecto(&1))
  end

  defp es_perfecto(n) do
    divisores = Enum.filter(1..n-1, &rem(n, &1) == 0)
    suma_divisores = Enum.reduce(divisores, 0, &Suma.suma(&1, &2))
    suma_divisores == n
  end
end
```

Este código es una colección de funciones complejas en Elixir que realizan diversas operaciones matemáticas. 

- La función `Suma` simplemente suma dos números.
- La función `SumaLista` suma todos los números de una lista.
- La función `Factorial` calcula el factorial de un número.
- La función `NumeroMayor` devuelve el número mayor de una lista.
- La función `NumeroMenor` devuelve el número menor de una lista.
- La función `NumerosPares` devuelve una lista con los números pares de una lista.
- La función `NumerosImpares` devuelve una lista con los números impares de una lista.
- La función `NumerosPrimos` devuelve una lista con los números primos de una lista.
- La función `NumerosPerfectos` devuelve una lista con los números perfectos de una lista.