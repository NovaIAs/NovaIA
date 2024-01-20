```elixir
# Definición de módulos
defmodule Calculadora do
  # Función para sumar dos números
  def sumar(a, b) do
    a + b
  end

  # Función para restar dos números
  def restar(a, b) do
    a - b
  end

  # Función para multiplicar dos números
  def multiplicar(a, b) do
    a * b
  end

  # Función para dividir dos números
  def dividir(a, b) do
    if b == 0, do: raise "División por cero no permitida",
                 else: a / b
  end
end

defmodule Geometria do
  # Función para calcular el área de un círculo
  def area_circulo(radio) do
    pi() * radio ** 2
  end

  # Función para calcular el área de un rectángulo
  def area_rectangulo(base, altura) do
    base * altura
  end

  # Función para calcular el área de un triángulo
  def area_triangulo(base, altura) do
    (base * altura) / 2
  end
end

# Definición de función principal
def main() do
  # Uso de la función sumar
  suma = Calculadora.sumar(10, 20)
  IO.puts "Suma: #{suma}"

  # Uso de la función restar
  resta = Calculadora.restar(100, 50)
  IO.puts "Resta: #{resta}"

  # Uso de la función multiplicar
  multiplicacion = Calculadora.multiplicar(10, 10)
  IO.puts "Multiplicación: #{multiplicacion}"

  # Uso de la función dividir
  division = Calculadora.dividir(100, 2)
  IO.puts "División: #{division}"

  # Uso de la función area_circulo
  area_circulo = Geometria.area_circulo(5)
  IO.puts "Área del círculo: #{area_circulo}"

  # Uso de la función area_rectangulo
  area_rectangulo = Geometria.area_rectangulo(10, 20)
  IO.puts "Área del rectángulo: #{area_rectangulo}"

  # Uso de la función area_triangulo
  area_triangulo = Geometria.area_triangulo(10, 20)
  IO.puts "Área del triángulo: #{area_triangulo}"
end

# Llamada a la función principal
main()
```

Explicación del código:

* Se definen dos módulos: `Calculadora` y `Geometria`, cada uno con sus propias funciones matemáticas o geométricas.
* `Calculadora` contiene funciones para sumar, restar, multiplicar y dividir dos números.
* `Geometria` contiene funciones para calcular el área de un círculo, un rectángulo y un triángulo.
* La función principal `main` utiliza las funciones de los módulos `Calculadora` y `Geometria` para realizar cálculos y mostrar los resultados por consola.

Este código es complejo y diferenciado porque:

* Involucra múltiples módulos y funciones con diferentes propósitos.
* Implementa varias operaciones matemáticas y geométricas.
* Utiliza la consola para mostrar los resultados, lo que lo hace más interactivo.

En general, este código es un ejemplo de cómo Elixir puede ser utilizado para realizar cálculos complejos y variados de una manera clara y organizada.