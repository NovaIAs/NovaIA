```elixir
defmodule Calculadora do
  def sumar(a, b) do
    a + b
  end

  def restar(a, b) do
    a - b
  end

  def multiplicar(a, b) do
    a * b
  end

  def dividir(a, b) do
    a / b
  end

  def potencia(a, b) do
    a ** b
  end

  def raiz_cuadrada(a) do
    :math.sqrt(a)
  end

  def raiz_cubica(a) do
    :math.cbrt(a)
  end

  def factorial(a) do
    if a == 0 do
      1
    else
      a * factorial(a - 1)
    end
  end

  def fibonacci(n) do
    if n < 2 do
      n
    else
      fibonacci(n - 1) + fibonacci(n - 2)
    end
  end

  def es_primo(n) do
    if n < 2 do
      false
    else
      divisor = 2
      while divisor * divisor <= n do
        if n % divisor == 0 do
          false
        else
          divisor += 1
        end
      end
      true
    end
  end

  def es_par(n) do
    n rem 2 == 0
  end

  def es_impar(n) do
    n rem 2 == 1
  end

  def es_positivo(n) do
    n > 0
  end

  def es_negativo(n) do
    n < 0
  end

  def es_cero(n) do
    n == 0
  end

  def abs(n) do
    if n < 0 do
      -n
    else
      n
    end
  end

  def redondear(n) do
    :math.round(n)
  end

  def trunc(n) do
    :math.trunc(n)
  end

  def ceil(n) do
    :math.ceil(n)
  end

  def floor(n) do
    :math.floor(n)
  end

  def mod(a, b) do
    a rem b
  end

  def max(a, b) do
    if a > b do
      a
    else
      b
    end
  end

  def min(a, b) do
    if a < b do
      a
    else
      b
    end
  end
end

IO.puts(Calculadora.sumar(1, 2))
IO.puts(Calculadora.restar(5, 3))
IO.puts(Calculadora.multiplicar(7, 8))
IO.puts(Calculadora.dividir(10, 2))
IO.puts(Calculadora.potencia(3, 2))
IO.puts(Calculadora.raiz_cuadrada(9))
IO.puts(Calculadora.raiz_cubica(27))
IO.puts(Calculadora.factorial(5))
IO.puts(Calculadora.fibonacci(8))
IO.puts(Calculadora.es_primo(17))
IO.puts(Calculadora.es_par(10))
IO.puts(Calculadora.es_impar(11))
IO.puts(Calculadora.es_positivo(5))
IO.puts(Calculadora.es_negativo(-3))
IO.puts(Calculadora.es_cero(0))
IO.puts(Calculadora.abs(-7))
IO.puts(Calculadora.redondear(3.14))
IO.puts(Calculadora.trunc(4.56))
IO.puts(Calculadora.ceil(2.71))
IO.puts(Calculadora.floor(6.89))
IO.puts(Calculadora.mod(13, 5))
IO.puts(Calculadora.max(25, 30))
IO.puts(Calculadora.min(15, 20))
```

Este código es una calculadora que realiza operaciones básicas y avanzadas.

El código está dividido en varias funciones, cada una de las cuales realiza una operación diferente.

Las funciones son las siguientes:

* `sumar`: suma dos números.
* `restar`: resta dos números.
* `multiplicar`: multiplica dos números.
* `dividir`: divide dos números.
* `potencia`: eleva un número a una potencia.
* `raiz_cuadrada`: calcula la raíz cuadrada de un número.
* `raiz_cubica`: calcula la raíz cúbica de un número.
* `factorial`: calcula el factorial de un número.
* `fibonacci`: calcula el número de Fibonacci de un número.
* `es_primo`: comprueba si un número es primo.
* `es_par`: comprueba si un número es par.
* `es_impar`: comprueba si un número es impar.
* `es_positivo`: comprueba si un número es positivo.
* `es_negativo`: comprueba si un número es negativo.
* `es_cero`: comprueba si un número es cero.
* `abs`: calcula el valor absoluto de un número.
* `redondear`: redondea un número al entero más cercano.
* `trunc`: trunca un número al entero más pequeño.
* `ceil`: eleva un número al entero más grande.
* `floor`: desciende un número al entero más pequeño.
* `mod`: calcula el resto de la división de dos números.
* `max`: devuelve el mayor de dos números.
* `min`: devuelve el menor de dos números.

El código también incluye una función `IO.puts` que se utiliza para imprimir los resultados de las operaciones.

Para utilizar la calculadora, simplemente hay que llamar a la función correspondiente y pasarle los valores que se desean operar. Por ejemplo, para sumar dos números, se puede llamar a la función `sumar` de la siguiente manera:

```
Calculadora.sumar(1, 2)
```

Esto devolverá el resultado de la suma de 1 y 2, que es 3.