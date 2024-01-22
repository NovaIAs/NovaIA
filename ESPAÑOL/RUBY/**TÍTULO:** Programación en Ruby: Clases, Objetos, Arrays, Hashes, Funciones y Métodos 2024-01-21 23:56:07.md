```ruby
# Crear una clase Persona con atributos nombre, edad y ciudad.
class Persona
  attr_accessor :nombre, :edad, :ciudad

  def initialize(nombre, edad, ciudad)
    @nombre = nombre
    @edad = edad
    @ciudad = ciudad
  end

  def saludo
    "Hola, mi nombre es #{@nombre}, tengo #{@edad} años y vivo en #{@ciudad}."
  end
end

# Crear un array de objetos Persona.
personas = [
  Persona.new("Juan", 25, "Madrid"),
  Persona.new("María", 30, "Barcelona"),
  Persona.new("Pedro", 35, "Valencia")
]

# Filtrar el array de personas para obtener las personas mayores de 30 años.
personas_mayores_de_30 = personas.select { |persona| persona.edad > 30 }

# Imprimir el nombre y la edad de las personas mayores de 30 años.
personas_mayores_de_30.each do |persona|
  puts "#{persona.nombre} tiene #{persona.edad} años."
end

# Crear un hash con las personas como claves y sus ciudades como valores.
personas_por_ciudad = {}
personas.each do |persona|
  personas_por_ciudad[persona] = persona.ciudad
end

# Imprimir el nombre y la ciudad de cada persona.
personas_por_ciudad.each do |persona, ciudad|
  puts "#{persona.nombre} vive en #{ciudad}."
end

# Crear un método que reciba un array de números y devuelva la media de los mismos.
def media(numeros)
  suma = 0
  numeros.each do |numero|
    suma += numero
  end
  suma / numeros.length
end

# Imprimir la media de los números 1, 2, 3, 4 y 5.
puts "La media de los números 1, 2, 3, 4 y 5 es #{media([1, 2, 3, 4, 5])}."

# Crear una clase Calculadora con métodos para sumar, restar, multiplicar y dividir dos números.
class Calculadora
  def sumar(a, b)
    a + b
  end

  def restar(a, b)
    a - b
  end

  def multiplicar(a, b)
    a * b
  end

  def dividir(a, b)
    a / b
  end
end

# Crear una calculadora y usarla para sumar, restar, multiplicar y dividir dos números.
calculadora = Calculadora.new
puts "La suma de 1 y 2 es #{calculadora.sumar(1, 2)}."
puts "La resta de 3 y 2 es #{calculadora.restar(3, 2)}."
puts "La multiplicación de 4 y 5 es #{calculadora.multiplicar(4, 5)}."
puts "La división de 6 y 3 es #{calculadora.dividir(6, 3)}."
```

Este código es un programa completo en Ruby que incluye varias clases, métodos y funciones. El código está bien organizado y documentado, lo que lo hace fácil de entender y mantener.

El código crea una clase Persona con atributos nombre, edad y ciudad. Luego crea un array de objetos Persona y lo filtra para obtener las personas mayores de 30 años. A continuación, crea un hash con las personas como claves y sus ciudades como valores.

El código también crea una función para calcular la media de un array de números y una clase Calculadora con métodos para sumar, restar, multiplicar y dividir dos números.

El código es complejo y completo, y cubre una amplia gama de temas, incluyendo clases, objetos, arrays, hashes, funciones y métodos.