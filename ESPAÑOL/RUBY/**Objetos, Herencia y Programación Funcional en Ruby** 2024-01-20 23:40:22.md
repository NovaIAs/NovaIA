```ruby
# Definición de una clase "Persona" con atributos y métodos básicos
class Persona
  attr_accessor :nombre, :apellido, :edad

  def initialize(nombre, apellido, edad)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
  end

  def saludar
    "Hola, mi nombre es #{@nombre} #{@apellido} y tengo #{@edad} años."
  end
end

# Definición de un módulo "Habilidades" con métodos útiles para personas
module Habilidades
  def programar
    "¡Puedo programar en Ruby!"
  end

  def hablar_idiomas(idiomas)
    "¡Puedo hablar #{idiomas.join(', ')}!"
  end
end

# Inclusión del módulo "Habilidades" en la clase "Persona"
class Persona
  include Habilidades
end

# Creación de una instancia de la clase "Persona"
persona1 = Persona.new("Juan", "García", 25)

# Utilización de los métodos de la clase "Persona" y el módulo "Habilidades"
puts persona1.saludar
puts persona1.programar
puts persona1.hablar_idiomas(["español", "inglés", "francés"])

# Definición de una clase "Estudiante" que hereda de la clase "Persona"
class Estudiante < Persona
  attr_accessor :universidad, :carrera

  def initialize(nombre, apellido, edad, universidad, carrera)
    super(nombre, apellido, edad) # Llamada al constructor de la clase padre
    @universidad = universidad
    @carrera = carrera
  end

  def estudiar
    "¡Estoy estudiando en la universidad #{universidad} la carrera #{carrera}!"
  end
end

# Creación de una instancia de la clase "Estudiante"
estudiante1 = Estudiante.new("María", "López", 22, "Universidad de Costa Rica", "Ingeniería en Computación")

# Utilización de los métodos de la clase "Estudiante" y la clase "Persona"
puts estudiante1.saludar
puts estudiante1.programar
puts estudiante1.hablar_idiomas(["español", "inglés"])
puts estudiante1.estudiar

# Utilización de bloques de código con el método "map" de un array
nombres_personas = ["Juan", "María", "Pedro", "Ana"]

nombres_en_mayúsculas = nombres_personas.map do |nombre|
  nombre.upcase
end

puts nombres_en_mayúsculas

# Utilización de la gema "awesome_print" para mostrar estructuras de datos de manera legible
require "awesome_print"

ap nombres_personas
ap nombres_en_mayúsculas
```

Explicación del código:

1. Definimos una clase "Persona" con atributos como nombre, apellido y edad. También definimos un método "saludar" que devuelve un saludo de la persona.

2. Creamos un módulo "Habilidades" que incluye métodos útiles como "programar" y "hablar_idiomas".

3. Incluimos el módulo "Habilidades" en la clase "Persona" para que las personas puedan utilizar esos métodos.

4. Creamos una instancia de la clase "Persona" y utilizamos sus métodos.

5. Definimos una clase "Estudiante" que hereda de la clase "Persona" y añadimos atributos como universidad y carrera.

6. Creamos una instancia de la clase "Estudiante" y utilizamos sus métodos.

7. Utilizamos bloques de código con el método "map" de un array para transformar cada elemento del array de acuerdo a una operación específica.

8. Utilizamos la gema "awesome_print" para mostrar estructuras de datos de manera legible.