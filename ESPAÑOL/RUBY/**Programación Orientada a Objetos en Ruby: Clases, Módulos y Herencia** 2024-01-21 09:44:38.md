```ruby
# Definición de clases y módulos

# Clase Persona
class Persona
  # Atributos de la clase
  attr_accessor :nombre, :apellido, :edad

  # Método para crear un objeto Persona
  def initialize(nombre, apellido, edad)
    self.nombre = nombre
    self.apellido = apellido
    self.edad = edad
  end

  # Método para obtener el nombre completo de la persona
  def nombre_completo
    "#{nombre} #{apellido}"
  end
end

# Módulo Direccion
module Direccion
  # Atributos del módulo
  attr_accessor :calle, :numero, :ciudad, :estado, :pais

  # Método para crear un objeto Dirección
  def initialize(calle, numero, ciudad, estado, pais)
    self.calle = calle
    self.numero = numero
    self.ciudad = ciudad
    self.estado = estado
    self.pais = pais
  end

  # Método para obtener la dirección completa
  def direccion_completa
    "#{calle} #{numero}, #{ciudad}, #{estado}, #{pais}"
  end
end

# Clase Estudiante, que hereda de Persona y usa el módulo Dirección
class Estudiante < Persona
  include Direccion

  # Atributos de la clase
  attr_accessor :matricula, :carrera

  # Método para crear un objeto Estudiante
  def initialize(nombre, apellido, edad, matricula, carrera, calle, numero, ciudad, estado, pais)
    super(nombre, apellido, edad)
    self.matricula = matricula
    self.carrera = carrera
    self.calle = calle
    self.numero = numero
    self.ciudad = ciudad
    self.estado = estado
    self.pais = pais
  end

  # Método para obtener la información completa del estudiante
  def informacion_completa
    "Nombre: #{nombre_completo}\n" \
    "Edad: #{edad}\n" \
    "Matrícula: #{matricula}\n" \
    "Carrera: #{carrera}\n" \
    "Dirección: #{direccion_completa}"
  end
end

# Creación de objetos

# Objeto Persona
persona1 = Persona.new("Juan", "Pérez", 30)

# Objeto Dirección
direccion1 = Direccion.new("Calle 1", "123", "Ciudad 1", "Estado 1", "País 1")

# Objeto Estudiante
estudiante1 = Estudiante.new("María", "Gómez", 20, "12345", "Ingeniería en Sistemas", "Calle 2", "456", "Ciudad 2", "Estado 2", "País 2")

# Impresión de información

# Información de la persona
puts "Persona 1:\n" \
     "Nombre: #{persona1.nombre_completo}\n" \
     "Edad: #{persona1.edad}"

# Información de la dirección
puts "\nDirección 1:\n" \
     "Dirección Completa: #{direccion1.direccion_completa}"

# Información del estudiante
puts "\nEstudiante 1:\n" \
     "#{estudiante1.informacion_completa}"
```

Explicación del código:

1. Se definen las clases Persona y Estudiante, y el módulo Dirección.

2. La clase Persona tiene atributos para el nombre, apellido y edad de una persona, y métodos para crear un objeto Persona y obtener el nombre completo.

3. El módulo Dirección tiene atributos para la calle, número, ciudad, estado y país de una dirección, y métodos para crear un objeto Dirección y obtener la dirección completa.

4. La clase Estudiante hereda de la clase Persona y usa el módulo Dirección. Además, tiene atributos para la matrícula y la carrera del estudiante, y métodos para crear un objeto Estudiante y obtener la información completa del estudiante.

5. Se crean objetos de las clases Persona, Dirección y Estudiante y se imprime información sobre cada objeto.