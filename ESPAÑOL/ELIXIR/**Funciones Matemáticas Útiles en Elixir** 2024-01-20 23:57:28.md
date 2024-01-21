```elixir
# definimos una función para saludar
defmodule Saluda do
  def saludar(nombre) do
    "Hola, #{nombre}!"
  end
end

# definimos una función para sumar dos números
defmodule Suma do
  def sumar(a, b) do
    a + b
  end
end

# definimos una función para calcular el factorial de un número
defmodule Factorial do
  def factorial(n) do
    if n == 0 do
      1
    else
      n * factorial(n-1)
    end
  end
end

# definimos una función para encontrar el máximo común divisor de dos números
defmodule MaximoComunDivisor do
  def mcd(a, b) do
    if b == 0 do
      a
    else
      mcd(b, rem(a,b))
    end
  end
end

# definimos una función para encontrar el mínimo común múltiplo de dos números
defmodule MinimoComunMultiplo do
  def mcm(a, b) do
    a * b / MaximoComunDivisor.mcd(a,b)
  end
end

# definimos una función para encontrar el menor número divisor de un número
defmodule MenorDivisor do
  def menor_divisor(n) do
    menor_divisor = 2
    while menor_divisor * menor_divisor <= n do
      if rem(n, menor_divisor) == 0 do
        return menor_divisor
      end
      menor_divisor += 1
    end
    n
  end
end

# definimos una función para encontrar el mayor número divisor de un número
defmodule MayorDivisor do
  def mayor_divisor(n) do
    mayor_divisor = n
    while mayor_divisor > 1 do
      if rem(n, mayor_divisor) == 0 do
        return mayor_divisor
      end
      mayor_divisor -= 1
    end
    1
  end
end
```

Este código contiene varias funciones matemáticas útiles. La función `saludar` simplemente saluda a alguien por su nombre. La función `sumar` suma dos números. La función `factorial` calcula el factorial de un número. La función `mcd` encuentra el máximo común divisor de dos números. La función `mcm` encuentra el mínimo común múltiplo de dos números. La función `menor_divisor` encuentra el menor número divisor de un número. La función `mayor_divisor` encuentra el mayor número divisor de un número.