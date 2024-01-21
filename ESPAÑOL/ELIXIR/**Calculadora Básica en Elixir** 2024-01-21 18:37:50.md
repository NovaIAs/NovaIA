```elixir
# Creación de un módulo para encapsular la funcionalidad
defmodule Calculadora do
  # Definición de una función para sumar dos números
  def sumar(a, b) do
    a + b
  end

  # Definición de una función para restar dos números
  def restar(a, b) do
    a - b
  end

  # Definición de una función para multiplicar dos números
  def multiplicar(a, b) do
    a * b
  end

  # Definición de una función para dividir dos números
  def dividir(a, b) do
    a / b
  end
end

# Llamadas a las funciones del módulo Calculadora
resultado_suma = Calculadora.sumar(10, 20)
resultado_resta = Calculadora.restar(30, 15)
resultado_multiplicacion = Calculadora.multiplicar(5, 10)
resultado_division = Calculadora.dividir(20, 5)

# Impresión de los resultados
IO.puts "Resultado Suma: #{resultado_suma}"
IO.puts "Resultado Resta: #{resultado_resta}"
IO.puts "Resultado Multiplicación: #{resultado_multiplicacion}"
IO.puts "Resultado División: #{resultado_division}"
```

Explicación del código:

1. Creamos un módulo llamado Calculadora para encapsular la funcionalidad relacionada con las operaciones matemáticas básicas.

2. Dentro del módulo, definimos cuatro funciones: sumar, restar, multiplicar y dividir, cada una de las cuales realiza la operación matemática correspondiente.

3. Invocamos a las funciones del módulo Calculadora para realizar las operaciones con los valores numéricos 10, 20, 30, 15, 5 y 10.

4. Imprimimos los resultados de las operaciones en la consola utilizando la función IO.puts.

Este código demuestra una forma de organizar código en Elixir usando módulos y funciones, y también ilustra cómo realizar operaciones matemáticas básicas en Elixir.