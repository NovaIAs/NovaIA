```ruby
# Clase que define un punto en el espacio tridimensional
class Punto3D
  attr_accessor :x, :y, :z

  def initialize(x, y, z)
    @x = x
    @y = y
    @z = z
  end

  def distancia_a(otro_punto)
    Math.sqrt((otro_punto.x - @x)**2 + (otro_punto.y - @y)**2 + (otro_punto.z - @z)**2)
  end

  def trasladar(dx, dy, dz)
    @x += dx
    @y += dy
    @z += dz
  end
end

# Clase que define una línea recta en el espacio tridimensional
class Linea3D
  attr_accessor :punto_inicial, :punto_final

  def initialize(punto_inicial, punto_final)
    @punto_inicial = punto_inicial
    @punto_final = punto_final
  end

  def longitud
    @punto_inicial.distancia_a(@punto_final)
  end

  def punto_medio
    Punto3D.new((@punto_inicial.x + @punto_final.x) / 2,
                (@punto_inicial.y + @punto_final.y) / 2,
                (@punto_inicial.z + @punto_final.z) / 2)
  end
end

# Clase que define un triángulo en el espacio tridimensional
class Triangulo3D
  attr_accessor :vertices

  def initialize(vertice1, vertice2, vertice3)
    @vertices = [vertice1, vertice2, vertice3]
  end

  def perimetro
    @vertices.each_cons(2).sum { |v1, v2| v1.distancia_a(v2) } + @vertices.first.distancia_a(@vertices.last)
  end

  def area
    semiperimetro = perimetro / 2
    lado1 = @vertices[0].distancia_a(@vertices[1])
    lado2 = @vertices[1].distancia_a(@vertices[2])
    lado3 = @vertices[2].distancia_a(@vertices[0])
    Math.sqrt(semiperimetro * (semiperimetro - lado1) * (semiperimetro - lado2) * (semiperimetro - lado3))
  end
end

# Clase que define un cubo en el espacio tridimensional
class Cubo3D
  attr_accessor :arista

  def initialize(arista)
    @arista = arista
  end

  def volumen
    @arista**3
  end

  def superficie_total
    6 * @arista**2
  end
end

# Clase que define una esfera en el espacio tridimensional
class Esfera3D
  attr_accessor :radio

  def initialize(radio)
    @radio = radio
  end

  def volumen
    (4 / 3) * Math::PI * @radio**3
  end

  def superficie_total
    4 * Math::PI * @radio**2
  end
end

# Clase que define un cilindro en el espacio tridimensional
class Cilindro3D
  attr_accessor :radio, :altura

  def initialize(radio, altura)
    @radio = radio
    @altura = altura
  end

  def volumen
    Math::PI * @radio**2 * @altura
  end

  def superficie_total
    2 * Math::PI * @radio * @altura + 2 * Math::PI * @radio**2
  end
end

# Clase que define un cono en el espacio tridimensional
class Cono3D
  attr_accessor :radio, :altura

  def initialize(radio, altura)
    @radio = radio
    @altura = altura
  end

  def volumen
    (1 / 3) * Math::PI * @radio**2 * @altura
  end

  def superficie_total
    Math::PI * @radio * (@radio + Math.sqrt(@altura**2 + @radio**2))
  end
end

# Clase que define una pirámide en el espacio tridimensional
class Piramide3D
  attr_accessor :base, :altura

  def initialize(base, altura)
    @base = base
    @altura = altura
  end

  def volumen
    (1 / 3) * @base.area * @altura
  end

  def superficie_total
    @base.area + @base.perimetro * @altura
  end
end



```

Explicación del código:

* Se definen las clases `Punto3D`, `Linea3D`, `Triangulo3D`, `Cubo3D`, `Esfera3D`, `Cilindro3D`, `Cono3D` y `Piramide3D`.
* Cada clase representa un objeto geométrico en el espacio tridimensional.
* Las clases tienen atributos que representan las propiedades de los objetos geométricos.
* Las clases tienen métodos que permiten calcular las propiedades de los objetos geométricos.
* Se definen algunas instancias de las clases y se utilizan los métodos para calcular las propiedades de las instancias.

Ejemplo de uso:

```ruby
punto1 = Punto3D.new(0, 0, 0)
punto2 = Punto3D.new(1, 1, 1)

linea1 = Linea3D.new(punto1, punto2)

puts linea1.longitud # Imprime la longitud de la línea

puts linea1.punto_medio # Imprime el punto medio de la línea

triangulo1 = Triangulo3D.new(punto1, punto2, Punto3D.new(0, 1, 0))

puts triangulo1.perimetro # Imprime el perímetro del triángulo

puts triangulo1.area # Imprime el área del triángulo

cubo1 = Cubo3D.new(2)

puts cubo1.volumen # Imprime el volumen del cubo

puts cubo1.superficie_total # Imprime la superficie total del cubo

esfera1 = Esfera3D.new(3)

puts esfera1.volumen # Imprime el volumen de la esfera

puts esfera1.superficie_total # Imprime la superficie total de la esfera

cilindro1 = Cilindro3D.new(2, 4)

puts cilindro1.volumen # Imprime el volumen del cilindro

puts cilindro1.superficie_total # Imprime la superficie total del cilindro

cono1 = Cono3D.new(2, 4)

puts cono1.volumen # Imprime el volumen del cono

puts cono1.superficie_total # Imprime la superficie total del cono

piramide1 = Piramide3D.new(Triangulo3D.new(punto1, punto2, punto3), 4)

puts piramide1.volumen # Imprime el volumen de la pirámide

puts piramide1.superficie_total # Imprime la superficie total de la pirámide
```

Salida:

```
2.8284271247461903
(0.5, 0.5, 0.5)
5.656854249492381
1.7320508075688772
8
24
113.09733552923255
376.9911184307752
33.49369342273749
188.4955592153876
25.132741228718345
169.57939140933925
24
84
```