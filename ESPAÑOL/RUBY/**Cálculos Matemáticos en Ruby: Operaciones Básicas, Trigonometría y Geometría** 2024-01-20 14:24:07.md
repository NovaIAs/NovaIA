```ruby
# Módulo para manejar las operaciones matemáticas básicas
module MatematicasBasicas

  # Método para sumar dos números
  def sumar(a, b)
    a + b
  end

  # Método para restar dos números
  def restar(a, b)
    a - b
  end

  # Método para multiplicar dos números
  def multiplicar(a, b)
    a * b
  end

  # Método para dividir dos números
  def dividir(a, b)
    a / b
  end

end

# Módulo para manejar las operaciones trigonométricas
module Trigonometria

  # Método para calcular el seno de un ángulo
  def seno(angulo)
    Math.sin(angulo)
  end

  # Método para calcular el coseno de un ángulo
  def coseno(angulo)
    Math.cos(angulo)
  end

  # Método para calcular la tangente de un ángulo
  def tangente(angulo)
    Math.tan(angulo)
  end

end

# Clase para representar un punto en el espacio
class Punto

  # Atributos para almacenar las coordenadas del punto
  attr_accessor :x, :y

  # Constructor para crear un nuevo punto
  def initialize(x, y)
    @x = x
    @y = y
  end

  # Método para calcular la distancia entre dos puntos
  def distancia(otro_punto)
    Math.sqrt((@x - otro_punto.x)**2 + (@y - otro_punto.y)**2)
  end

end

# Clase para representar una línea recta
class LineaRecta

  # Atributos para almacenar los coeficientes de la ecuación de la línea
  attr_accessor :a, :b, :c

  # Constructor para crear una nueva línea recta
  def initialize(a, b, c)
    @a = a
    @b = b
    @c = c
  end

  # Método para calcular la intersección de dos líneas rectas
  def interseccion(otra_linea)
    x = (otra_linea.c - @c) / (@a - otra_linea.a)
    y = (-@a * x - @c) / @b
    Punto.new(x, y)
  end

end

# Incluir los módulos en la clase principal
class Calculadora
  include MatematicasBasicas
  include Trigonometria
end

# Crear una instancia de la clase Calculadora
calculadora = Calculadora.new

# Utilizar los métodos de la clase Calculadora
suma = calculadora.sumar(1, 2)
resta = calculadora.restar(3, 4)
multiplicacion = calculadora.multiplicar(5, 6)
division = calculadora.dividir(7, 8)

seno = calculadora.seno(Math::PI / 2)
coseno = calculadora.coseno(Math::PI / 2)
tangente = calculadora.tangente(Math::PI / 4)

punto_a = Punto.new(1, 2)
punto_b = Punto.new(3, 4)
distancia = punto_a.distancia(punto_b)

linea_a = LineaRecta.new(1, 2, 3)
linea_b = LineaRecta.new(2, 3, 4)
interseccion = linea_a.interseccion(linea_b)

# Mostrar los resultados
puts "Suma: #{suma}"
puts "Resta: #{resta}"
puts "Multiplicación: #{multiplicacion}"
puts "División: #{division}"

puts "Seno: #{seno}"
puts "Coseno: #{coseno}"
puts "Tangente: #{tangente}"

puts "Distancia entre punto A y punto B: #{distancia}"

puts "Intersección entre línea A y línea B: #{interseccion.x}, #{interseccion.y}"
```

Este código es un ejemplo de un código complejo en RUBY que incluye varias clases y módulos, y que realiza una variedad de cálculos matemáticos. El código está bien documentado y es fácil de entender.