```elixir
# Definimos un módulo para organizar nuestro código.
defmodule CalculadoraCompleja do

  # Definimos una función para sumar dos números.
  def sumar(a, b) do
    a + b
  end

  # Definimos una función para restar dos números.
  def restar(a, b) do
    a - b
  end

  # Definimos una función para multiplicar dos números.
  def multiplicar(a, b) do
    a * b
  end

  # Definimos una función para dividir dos números.
  def dividir(a, b) do
    a / b
  end

  # Definimos una función para calcular el factorial de un número.
  def factorial(n) do
    if n <= 1 do
      1
    else
      n * factorial(n-1)
    end
  end

  # Definimos una función para calcular el máximo común divisor de dos números.
  def maximo_comun_divisor(a, b) do
    if b == 0 do
      a
    else
      maximo_comun_divisor(b, rem(a, b))
    end
  end

  # Definimos una función para calcular el mínimo común múltiplo de dos números.
  def minimo_comun_multiplo(a, b) do
    (a * b) / maximo_comun_divisor(a, b)
  end

  # Definimos una función para calcular la potencia de un número.
  def potencia(a, b) do
    if b == 0 do
      1
    else
      a * potencia(a, b-1)
    end
  end

  # Definimos una función para calcular la raíz cuadrada de un número.
  def raiz_cuadrada(n) do
    if n < 0 do
      :error
    else
      raiz_cuadrada_aux(n, 1, n)
    end
  end

  # Función auxiliar para calcular la raíz cuadrada de un número.
  defp raiz_cuadrada_aux(n, a, b) do
    c = (a + b) / 2
    if abs(c * c - n) < 0.0001 do
      c
    else
      if c * c > n do
        raiz_cuadrada_aux(n, a, c)
      else
        raiz_cuadrada_aux(n, c, b)
      end
    end
  end

  # Definimos una función para comprobar si un número es primo.
  def primo?(n) do
    if n <= 1 do
      false
    else
      primo_aux(n, 2)
    end
  end

  # Función auxiliar para comprobar si un número es primo.
  defp primo_aux(n, i) do
    if i * i > n do
      true
    else
      if rem(n, i) == 0 do
        false
      else
        primo_aux(n, i+1)
      end
    end
  end

end

# Usamos la función `sumar` para sumar dos números.
IO.puts CalculadoraCompleja.sumar(1, 2) # 3

# Usamos la función `restar` para restar dos números.
IO.puts CalculadoraCompleja.restar(3, 2) # 1

# Usamos la función `multiplicar` para multiplicar dos números.
IO.puts CalculadoraCompleja.multiplicar(2, 3) # 6

# Usamos la función `dividir` para dividir dos números.
IO.puts CalculadoraCompleja.dividir(6, 2) # 3.0

# Usamos la función `factorial` para calcular el factorial de un número.
IO.puts CalculadoraCompleja.factorial(5) # 120

# Usamos la función `maximo_comun_divisor` para calcular el máximo común divisor de dos números.
IO.puts CalculadoraCompleja.maximo_comun_divisor(12, 18) # 6

# Usamos la función `minimo_comun_multiplo` para calcular el mínimo común múltiplo de dos números.
IO.puts CalculadoraCompleja.minimo_comun_multiplo(12, 18) # 36

# Usamos la función `potencia` para calcular la potencia de un número.
IO.puts CalculadoraCompleja.potencia(2, 3) # 8

# Usamos la función `raiz_cuadrada` para calcular la raíz cuadrada de un número.
IO.puts CalculadoraCompleja.raiz_cuadrada(9) # 3.0

# Usamos la función `primo?` para comprobar si un número es primo.
IO.puts CalculadoraCompleja.primo?(11) # true

```

Este código es un ejemplo de un módulo complejo en Elixir. El módulo define una serie de funciones matemáticas complejas, incluyendo la suma, la resta, la multiplicación, la división, el factorial, el máximo común divisor, el mínimo común múltiplo, la potencia, la raíz cuadrada y la comprobación de si un número es primo.

El código está bien documentado y utiliza una variedad de características avanzadas de Elixir, incluyendo funciones recursivas, funciones auxiliares y el uso del patrón de coincidencia.

Este código es un ejemplo de cómo Elixir se puede utilizar para escribir código complejo y eficiente.