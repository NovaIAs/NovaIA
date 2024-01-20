```ruby
# Definir una clase abstracta para representar una forma geométrica
class FormaGeometrica
  # Método abstracto para calcular el área
  def area; raise NotImplementedError; end

  # Método abstracto para calcular el perímetro
  def perimetro; raise NotImplementedError; end
end

# Definir una clase para representar un círculo
class Circulo < FormaGeometrica
  attr_reader :radio

  # Constructor que recibe el radio del círculo
  def initialize(radio)
    @radio = radio
  end

  # Implementar el método para calcular el área
  def area
    Math::PI * @radio ** 2
  end

  # Implementar el método para calcular el perímetro
  def perimetro
    2 * Math::PI * @radio
  end
end

# Definir una clase para representar un rectángulo
class Rectangulo < FormaGeometrica
  attr_reader :ancho, :alto

  # Constructor que recibe el ancho y el alto del rectángulo
  def initialize(ancho, alto)
    @ancho = ancho
    @alto = alto
  end

  # Implementar el método para calcular el área
  def area
    @ancho * @alto
  end

  # Implementar el método para calcular el perímetro
  def perimetro
    2 * (@ancho + @alto)
  end
end

# Definir una clase para representar un triángulo
class Triangulo < FormaGeometrica
  attr_reader :base, :altura

  # Constructor que recibe la base y la altura del triángulo
  def initialize(base, altura)
    @base = base
    @altura = altura
  end

  # Implementar el método para calcular el área
  def area
    0.5 * @base * @altura
  end

  # Implementar el método para calcular el perímetro
  def perimetro
    @base + @altura + Math.hypot(@base, @altura)
  end
end

# Definir un módulo para representar un dibujante de formas geométricas
module Dibujante
  # Método para dibujar una forma geométrica en la consola
  def dibujar(forma)
    puts "Dibujando una #{forma.class.name}:"
    puts "  Área: #{forma.area}"
    puts "  Perímetro: #{forma.perimetro}"
  end
end

# Incluir el módulo Dibujante en la clase principal
class Main
  include Dibujante

  # Método principal que crea y dibuja las formas geométricas
  def main
    # Crear un círculo con un radio de 5
    circulo = Circulo.new(5)

    # Crear un rectángulo con un ancho de 10 y una altura de 8
    rectangulo = Rectangulo.new(10, 8)

    # Crear un triángulo con una base de 6 y una altura de 4
    triangulo = Triangulo.new(6, 4)

    # Dibujar las formas geométricas
    dibujar(circulo)
    dibujar(rectangulo)
    dibujar(triangulo)
  end
end

# Crear una instancia de la clase principal y ejecutar el método principal
Main.new.main
```

Explicación del código:

* Se define una clase abstracta `FormaGeometrica` que representa una forma geométrica. Esta clase tiene dos métodos abstractos: `area` y `perímetro`, que deben ser implementados por las clases hijas.
* Se definen tres clases hijas de `FormaGeometrica`: `Circulo`, `Rectangulo` y `Triangulo`. Cada una de estas clases implementa los métodos `area` y `perímetro` para calcular el área y el perímetro de la forma geométrica correspondiente.
* Se define un módulo `Dibujante` que contiene un método `dibujar` que se utiliza para dibujar una forma geométrica en la consola.
* Se incluye el módulo `Dibujante` en la clase principal `Main`.
* El método principal `main` crea y dibuja tres formas geométricas: un círculo, un rectángulo y un triángulo.
* Se crea una instancia de la clase principal `Main` y se ejecuta el método principal `main`.