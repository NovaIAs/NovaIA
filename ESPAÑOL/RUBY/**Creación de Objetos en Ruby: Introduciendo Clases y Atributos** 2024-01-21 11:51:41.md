```ruby
# Define una clase de vehículo con atributos para el modelo, la marca y el año.
class Vehículo
  attr_accessor :modelo, :marca, :año

  def initialize(modelo, marca, año)
    @modelo = modelo
    @marca = marca
    @año = año
  end

  # Define un método para imprimir los datos del vehículo.
  def imprimir
    puts "Modelo: #{@modelo}"
    puts "Marca: #{@marca}"
    puts "Año: #{@año}"
  end
end

# Crea una instancia de la clase Vehículo.
mi_vehículo = Vehículo.new("Corolla", "Toyota", 2020)

# Imprime los datos del vehículo.
mi_vehículo.imprimir

# Define una clase de persona con atributos para el nombre y la edad.
class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  # Define un método para imprimir los datos de la persona.
  def imprimir
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
  end
end

# Crea una instancia de la clase Persona.
mi_persona = Persona.new("Juan Pérez", 30)

# Imprime los datos de la persona.
mi_persona.imprimir

# Define una clase de animal con atributos para el tipo y la edad.
class Animal
  attr_accessor :tipo, :edad

  def initialize(tipo, edad)
    @tipo = tipo
    @edad = edad
  end

  # Define un método para imprimir los datos del animal.
  def imprimir
    puts "Tipo: #{@tipo}"
    puts "Edad: #{@edad}"
  end
end

# Crea una instancia de la clase Animal.
mi_animal = Animal.new("Perro", 5)

# Imprime los datos del animal.
mi_animal.imprimir
```

Este código está escrito en formato ERB y puede ser utilizado en una aplicación web basada en Ruby on Rails.

El código define tres clases: Vehículo, Persona y Animal.

Cada una de las clases tiene atributos y métodos específicos.

Por ejemplo, la clase Vehículo tiene atributos para el modelo, la marca y el año, y un método para imprimir los datos del vehículo.

La clase Persona tiene atributos para el nombre y la edad, y un método para imprimir los datos de la persona.

La clase Animal tiene atributos para el tipo y la edad, y un método para imprimir los datos del animal.

Al final del código se crean instancias de las tres clases y se imprimen los datos de cada una de ellas.