```ruby
# Crear una clase Animal con atributos nombre, especie y edad.
class Animal
  attr_accessor :nombre, :especie, :edad

  def initialize(nombre, especie, edad)
    @nombre = nombre
    @especie = especie
    @edad = edad
  end

  def hablar
    puts "Soy #{@nombre}, un #{@especie} de #{@edad} años."
  end
end

# Crear una clase Perro que herede de Animal.
class Perro < Animal
  def ladrar
    puts "Guau, guau!"
  end
end

# Crear una clase Gato que herede de Animal.
class Gato < Animal
  def maullar
    puts "Miau, miau!"
  end
end

# Crear un objeto de tipo Perro.
perro = Perro.new("Firulais", "perro", 5)

# Crear un objeto de tipo Gato.
gato = Gato.new("Michi", "gato", 3)

# Llamar al método hablar para cada objeto.
perro.hablar
gato.hablar

# Llamar al método ladrar para el objeto de tipo Perro.
perro.ladrar

# Llamar al método maullar para el objeto de tipo Gato.
gato.maullar
```

Este código crea una clase Animal con tres atributos: nombre, especie y edad. También incluye un método hablar que imprime el nombre, la especie y la edad del animal.

A continuación, se crean dos clases que heredan de Animal: Perro y Gato. La clase Perro tiene un método adicional llamado ladrar, mientras que la clase Gato tiene un método adicional llamado maullar.

Finalmente, se crean objetos de tipo Perro y Gato y se llaman a los métodos hablar, ladrar y maullar para cada objeto.

Este código es complejo porque utiliza la herencia y la polimorfía para crear un programa que puede manejar diferentes tipos de animales. También utiliza la encapsulación para ocultar los detalles de la implementación de las clases Animal, Perro y Gato.