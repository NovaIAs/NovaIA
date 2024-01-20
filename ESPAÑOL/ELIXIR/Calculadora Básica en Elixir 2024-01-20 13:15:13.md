```
# Definimos un módulo para organizar nuestro código.
defmodule Calculadora do

  # Creamos una función para sumar dos números.
  def sumar(a, b) do
    a + b
  end

  # Creamos una función para restar dos números.
  def restar(a, b) do
    a - b
  end

  # Creamos una función para multiplicar dos números.
  def multiplicar(a, b) do
    a * b
  end

  # Creamos una función para dividir dos números.
  def dividir(a, b) do
    a / b
  end

  # Creamos una función para calcular el módulo de dos números.
  def modulo(a, b) do
    rem(a, b)
  end

  # Creamos una función para calcular el exponente de un número.
  def exponente(a, b) do
    :math.pow(a, b)
  end

  # Creamos una función para calcular el logaritmo de un número.
  def logaritmo(a) do
    :math.log(a)
  end

  # Creamos una función para calcular la raíz cuadrada de un número.
  def raiz_cuadrada(a) do
    :math.sqrt(a)
  end

  # Creamos una función para calcular el seno de un ángulo.
  def seno(a) do
    :math.sin(a)
  end

  # Creamos una función para calcular el coseno de un ángulo.
  def coseno(a) do
    :math.cos(a)
  end

  # Creamos una función para calcular la tangente de un ángulo.
  def tangente(a) do
    :math.tan(a)
  end

  # Creamos una función para calcular el arco seno de un número.
  def arco_seno(a) do
    :math.asin(a)
  end

  # Creamos una función para calcular el arco coseno de un número.
  def arco_coseno(a) do
    :math.acos(a)
  end

  # Creamos una función para calcular el arco tangente de un número.
  def arco_tangente(a) do
    :math.atan(a)
  end

end

# Usamos la función `sumar` para sumar dos números.
IO.puts(Calculadora.sumar(1, 2)) # Imprime: 3

# Usamos la función `restar` para restar dos números.
IO.puts(Calculadora.restar(3, 2)) # Imprime: 1

# Usamos la función `multiplicar` para multiplicar dos números.
IO.puts(Calculadora.multiplicar(2, 3)) # Imprime: 6

# Usamos la función `dividir` para dividir dos números.
IO.puts(Calculadora.dividir(6, 2)) # Imprime: 3.0

# Usamos la función `modulo` para calcular el módulo de dos números.
IO.puts(Calculadora.modulo(7, 3)) # Imprime: 1

# Usamos la función `exponente` para calcular el exponente de un número.
IO.puts(Calculadora.exponente(2, 3)) # Imprime: 8

# Usamos la función `logaritmo` para calcular el logaritmo de un número.
IO.puts(Calculadora.logaritmo(10)) # Imprime: 2.302585092994046

# Usamos la función `raiz_cuadrada` para calcular la raíz cuadrada de un número.
IO.puts(Calculadora.raiz_cuadrada(9)) # Imprime: 3.0

# Usamos la función `seno` para calcular el seno de un ángulo.
IO.puts(Calculadora.seno(30 * :math.pi / 180)) # Imprime: 0.5

# Usamos la función `coseno` para calcular el coseno de un ángulo.
IO.puts(Calculadora.coseno(30 * :math.pi / 180)) # Imprime: 0.8660254037844386

# Usamos la función `tangente` para calcular la tangente de un ángulo.
IO.puts(Calculadora.tangente(30 * :math.pi / 180)) # Imprime: 0.5773502691896257

# Usamos la función `arco_seno` para calcular el arco seno de un número.
IO.puts(Calculadora.arco_seno(0.5)) # Imprime: 30.0 * :math.pi / 180

# Usamos la función `arco_coseno` para calcular el arco coseno de un número.
IO.puts(Calculadora.arco_coseno(0.8660254037844386)) # Imprime: 30.0 * :math.pi / 180

# Usamos la función `arco_tangente` para calcular el arco tangente de un número.
IO.puts(Calculadora.arco_tangente(0.5773502691896257)) # Imprime: 30.0 * :math.pi / 180

```

Explicación del código:

1. Definimos un módulo llamado `Calculadora` para organizar nuestro código.
2. Creamos una función llamada `sumar` para sumar dos números.
3. Creamos una función llamada `restar` para restar dos números.
4. Creamos una función llamada `multiplicar` para multiplicar dos números.
5. Creamos una función llamada `dividir` para dividir dos números.
6. Creamos una función llamada `modulo` para calcular el módulo de dos números.
7. Creamos una función llamada `exponente` para calcular el exponente de un número.
8. Creamos una función llamada `logaritmo` para calcular el logaritmo de un número.
9. Creamos una función llamada `raiz_cuadrada` para calcular la raíz cuadrada de un número.
10. Creamos una función llamada `seno` para calcular el seno de un ángulo.
11. Creamos una función llamada `coseno` para calcular el coseno de un ángulo.
12. Creamos una función llamada `tangente` para calcular la tangente de un ángulo.
13. Creamos una función llamada `arco_seno` para calcular el arco seno de un número.
14. Creamos una función llamada `arco_coseno` para calcular el arco coseno de un número.
15. Creamos una función llamada `arco_tangente` para calcular el arco tangente de un número.
16. Usamos las funciones que creamos para calcular diferentes operaciones matemáticas.