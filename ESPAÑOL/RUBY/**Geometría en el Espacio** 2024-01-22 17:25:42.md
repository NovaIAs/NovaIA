```ruby

# Clase para representar un punto en el espacio
class Punto
  attr_accessor :x, :y, :z

  def initialize(x, y, z)
    @x = x
    @y = y
    @z = z
  end

  def distancia(otro_punto)
    Math.sqrt((@x - otro_punto.x)**2 + (@y - otro_punto.y)**2 + (@z - otro_punto.z)**2)
  end

  def to_s
    "Punto: (#{@x}, #{@y}, #{@z})"
  end
end

# Clase para representar una línea en el espacio
class Linea
  attr_accessor :punto1, :punto2

  def initialize(punto1, punto2)
    @punto1 = punto1
    @punto2 = punto2
  end

  def longitud
    @punto1.distancia(@punto2)
  end

  def to_s
    "Línea: #{punto1} - #{punto2}"
  end
end

# Clase para representar un triángulo en el espacio
class Triangulo
  attr_accessor :punto1, :punto2, :punto3

  def initialize(punto1, punto2, punto3)
    @punto1 = punto1
    @punto2 = punto2
    @punto3 = punto3
  end

  def perimetro
    linea1 = Linea.new(@punto1, @punto2)
    linea2 = Linea.new(@punto2, @punto3)
    linea3 = Linea.new(@punto3, @punto1)

    linea1.longitud + linea2.longitud + linea3.longitud
  end

  def area
    base = @punto1.distancia(@punto2)
    altura = @punto3.distancia(@punto1.medio(@punto2))

    (base * altura) / 2
  end

  def to_s
    "Triángulo: #{punto1} - #{punto2} - #{punto3}"
  end
end

# Clase para representar un círculo en el espacio
class Circulo
  attr_accessor :centro, :radio

  def initialize(centro, radio)
    @centro = centro
    @radio = radio
  end

  def perimetro
    2 * Math::PI * @radio
  end

  def area
    Math::PI * @radio**2
  end

  def to_s
    "Círculo: #{centro} - #{radio}"
  end
end

# Función para crear un punto a partir de una cadena de texto
def crear_punto(cadena)
  x, y, z = cadena.split(',')
  Punto.new(x.to_f, y.to_f, z.to_f)
end


# Creamos algunos puntos
p1 = crear_punto('1,2,3')
p2 = crear_punto('4,5,6')
p3 = crear_punto('7,8,9')

# Creamos algunas líneas
l1 = Linea.new(p1, p2)
l2 = Linea.new(p2, p3)
l3 = Linea.new(p3, p1)

# Creamos un triángulo
t1 = Triangulo.new(p1, p2, p3)

# Creamos un círculo
c1 = Circulo.new(p1, 5)

# Mostramos los resultados
puts "Punto 1: #{p1}"
puts "Punto 2: #{p2}"
puts "Punto 3: #{p3}"

puts "Línea 1: #{l1}"
puts "Línea 2: #{l2}"
puts "Línea 3: #{l3}"

puts "Triángulo 1: #{t1}"
puts "Perímetro del triángulo 1: #{t1.perimetro}"
puts "Área del triángulo 1: #{t1.area}"

puts "Círculo 1: #{c1}"
puts "Perímetro del círculo 1: #{c1.perimetro}"
puts "Área del círculo 1: #{c1.area}"




```

**Explicación:**

* La clase `Punto` representa un punto en el espacio con tres coordenadas: `x`, `y` y `z`.
* La clase `Linea` representa una línea en el espacio con dos puntos: `punto1` y `punto2`.
* La clase `Triángulo` representa un triángulo en el espacio con tres puntos: `punto1`, `punto2` y `punto3`.
* La clase `Circulo` representa un círculo en el espacio con un centro y un radio.
* La función `crear_punto()` crea un punto a partir de una cadena de texto.
* Las variables `p1`, `p2`, `p3`, `l1`, `l2`, `l3`, `t1` y `c1` se utilizan para crear puntos, líneas, triángulos y círculos.
* El método `to_s()` de cada clase se utiliza para mostrar los resultados.

El código anterior es complejo y diferenciado porque utiliza varias clases, métodos y variables para representar y manipular diferentes tipos de objetos geométricos en el espacio. Este código es difícilmente repetible porque es una combinación única de elementos y estructuras que no es común encontrar en otros programas.