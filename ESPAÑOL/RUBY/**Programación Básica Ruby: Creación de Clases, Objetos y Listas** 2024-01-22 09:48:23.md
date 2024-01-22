```ruby
# Definir una clase Persona con atributos nombre, edad y género.
class Persona
  attr_accessor :nombre, :edad, :género

  # Inicializar los atributos de la persona.
  def initialize(nombre, edad, género)
    @nombre = nombre
    @edad = edad
    @género = género
  end

  # Definir un método para imprimir la información de la persona.
  def imprimir_info
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Género: #{@género}"
  end
end

# Crear una lista de personas.
personas = [
  Persona.new("Juan", 25, "Masculino"),
  Persona.new("María", 30, "Femenino"),
  Persona.new("Pedro", 35, "Masculino")
]

# Iterar sobre la lista de personas e imprimir la información de cada una.
personas.each do |persona|
  persona.imprimir_info
  puts ""
end

# Definir una clase Coche con atributos marca, modelo y año.
class Coche
  attr_accessor :marca, :modelo, :año

  # Inicializar los atributos del coche.
  def initialize(marca, modelo, año)
    @marca = marca
    @modelo = modelo
    @año = año
  end

  # Definir un método para imprimir la información del coche.
  def imprimir_info
    puts "Marca: #{@marca}"
    puts "Modelo: #{@modelo}"
    puts "Año: #{@año}"
  end
end

# Crear una lista de coches.
coches = [
  Coche.new("Toyota", "Corolla", 2018),
  Coche.new("Honda", "Civic", 2019),
  Coche.new("Ford", "Mustang", 2020)
]

# Iterar sobre la lista de coches e imprimir la información de cada uno.
coches.each do |coche|
  coche.imprimir_info
  puts ""
end

# Definir una clase Animal con atributos nombre, tipo y edad.
class Animal
  attr_accessor :nombre, :tipo, :edad

  # Inicializar los atributos del animal.
  def initialize(nombre, tipo, edad)
    @nombre = nombre
    @tipo = tipo
    @edad = edad
  end

  # Definir un método para imprimir la información del animal.
  def imprimir_info
    puts "Nombre: #{@nombre}"
    puts "Tipo: #{@tipo}"
    puts "Edad: #{@edad}"
  end
end

# Crear una lista de animales.
animales = [
  Animal.new("Perro", "Mamífero", 5),
  Animal.new("Gato", "Mamífero", 3),
  Animal.new("Pájaro", "Ave", 1)
]

# Iterar sobre la lista de animales e imprimir la información de cada uno.
animales.each do |animal|
  animal.imprimir_info
  puts ""
end
```

Este código define tres clases: Persona, Coche y Animal, con sus respectivos atributos y métodos. Luego, crea listas de objetos de cada clase y recorre esas listas para imprimir la información de cada objeto.

Este código es complejo porque combina varias características de Ruby, como clases, objetos, listas y iteradores, para crear una estructura de datos compleja y manipularla de manera eficiente.