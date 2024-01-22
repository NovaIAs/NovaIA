```elixir
defmodule Calculadora do
  def suma(a, b) do
    a + b
  end

  def resta(a, b) do
    a - b
  end

  def multiplicacion(a, b) do
    a * b
  end

  def division(a, b) do
    a / b
  end

  def potencia(a, b) do
    :math.pow(a, b)
  end

  def raiz_cuadrada(a) do
    :math.sqrt(a)
  end

  def factorial(n) do
    if n == 0 do
      1
    else
      n * factorial(n-1)
    end
  end

  def es_primo(n) do
    if n <= 1 do
      false
    else
      es_primo_aux(n, 2)
    end
  end

  defp es_primo_aux(n, divisor) do
    if divisor == n do
      true
    else
      if rem(n, divisor) == 0 do
        false
      else
        es_primo_aux(n, divisor+1)
      end
    end
  end

  def fibonacci(n) do
    if n <= 1 do
      n
    else
      fibonacci(n-1) + fibonacci(n-2)
    end
  end

  def area_circulo(radio) do
    :math.pi * radio**2
  end

  def perimetro_circulo(radio) do
    2 * :math.pi * radio
  end

  def area_rectangulo(base, altura) do
    base * altura
  end

  def perimetro_rectangulo(base, altura) do
    2 * (base + altura)
  end

  def area_triangulo(base, altura) do
    (base * altura) / 2
  end

  def perimetro_triangulo(lado1, lado2, lado3) do
    lado1 + lado2 + lado3
  end
end

# Ejemplo de uso
IO.puts "La suma de 1 y 2 es: #{Calculadora.suma(1, 2)}"
IO.puts "La resta de 3 y 4 es: #{Calculadora.resta(3, 4)}"
IO.puts "La multiplicación de 5 y 6 es: #{Calculadora.multiplicacion(5, 6)}"
IO.puts "La división de 10 y 2 es: #{Calculadora.division(10, 2)}"
IO.puts "La potencia de 2 elevado a 3 es: #{Calculadora.potencia(2, 3)}"
IO.puts "La raíz cuadrada de 9 es: #{Calculadora.raiz_cuadrada(9)}"
IO.puts "El factorial de 5 es: #{Calculadora.factorial(5)}"
IO.puts "El número 7 es primo?: #{Calculadora.es_primo(7)}"
IO.puts "El número 10 es primo?: #{Calculadora.es_primo(10)}"
IO.puts "El término 10 de la serie de Fibonacci es: #{Calculadora.fibonacci(10)}"
IO.puts "El área de un círculo con radio 5 es: #{Calculadora.area_circulo(5)}"
IO.puts "El perímetro de un círculo con radio 5 es: #{Calculadora.perimetro_circulo(5)}"
IO.puts "El área de un rectángulo con base 10 y altura 5 es: #{Calculadora.area_rectangulo(10, 5)}"
IO.puts "El perímetro de un rectángulo con base 10 y altura 5 es: #{Calculadora.perimetro_rectangulo(10, 5)}"
IO.puts "El área de un triángulo con base 10 y altura 5 es: #{Calculadora.area_triangulo(10, 5)}"
IO.puts "El perímetro de un triángulo con lados 3, 4 y 5 es: #{Calculadora.perimetro_triangulo(3, 4, 5)}"
```

Explicación del código:

* El módulo `Calculadora` define una serie de funciones para realizar operaciones matemáticas básicas y avanzadas.
* Las funciones `suma`, `resta`, `multiplicacion` y `division` realizan las operaciones aritméticas básicas.
* Las funciones `potencia`, `raiz_cuadrada` y `factorial` realizan operaciones matemáticas más avanzadas.
* Las funciones `es_primo` y `fibonacci` realizan cálculos matemáticos específicos.
* Las funciones `area_circulo`, `perimetro_circulo`, `area_rectangulo`, `perimetro_rectangulo`, `area_triangulo` y `perimetro_triangulo` realizan cálculos geométricos.
* El código de ejemplo al final del módulo muestra cómo utilizar las funciones definidas en el módulo `Calculadora`.