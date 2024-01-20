```ruby
# Definir una clase llamada "Persona"
class Persona
  # Definir los atributos de la clase "Persona"
  attr_accessor :nombre, :edad, :sexo

  # Definir el método "initialize" para inicializar los atributos de un objeto
  def initialize(nombre, edad, sexo)
    @nombre = nombre
    @edad = edad
    @sexo = sexo
  end

  # Definir el método "to_s" para obtener una representación en cadena de texto de un objeto
  def to_s
    "Persona: nombre = #{@nombre}, edad = #{@edad}, sexo = #{@sexo}"
  end
end

# Definir una clase llamada "Estudiante" que hereda de la clase "Persona"
class Estudiante < Persona
  # Definir los atributos de la clase "Estudiante"
  attr_accessor :matricula, :promedio

  # Definir el método "initialize" para inicializar los atributos de un objeto
  def initialize(nombre, edad, sexo, matricula, promedio)
    super(nombre, edad, sexo)
    @matricula = matricula
    @promedio = promedio
  end

  # Definir el método "to_s" para obtener una representación en cadena de texto de un objeto
  def to_s
    "Estudiante: nombre = #{@nombre}, edad = #{@edad}, sexo = #{@sexo}, matricula = #{@matricula}, promedio = #{@promedio}"
  end
end

# Definir una clase llamada "Profesor" que hereda de la clase "Persona"
class Profesor < Persona
  # Definir los atributos de la clase "Profesor"
  attr_accessor :asignatura, :sueldo

  # Definir el método "initialize" para inicializar los atributos de un objeto
  def initialize(nombre, edad, sexo, asignatura, sueldo)
    super(nombre, edad, sexo)
    @asignatura = asignatura
    @sueldo = sueldo
  end

  # Definir el método "to_s" para obtener una representación en cadena de texto de un objeto
  def to_s
    "Profesor: nombre = #{@nombre}, edad = #{@edad}, sexo = #{@sexo}, asignatura = #{@asignatura}, sueldo = #{@sueldo}"
  end
end

# Crear un objeto de la clase "Persona"
persona = Persona.new("Juan", 20, "Masculino")

# Crear un objeto de la clase "Estudiante"
estudiante = Estudiante.new("María", 18, "Femenino", "2023-01-1", 8.5)

# Crear un objeto de la clase "Profesor"
profesor = Profesor.new("Pedro", 40, "Masculino", "Matemáticas", 2500)

# Imprimir los objetos en la consola
puts persona
puts estudiante
puts profesor
```

Este código define tres clases en Ruby: "Persona", "Estudiante" y "Profesor". La clase "Persona" define los atributos comunes a todas las personas, como el nombre, la edad y el sexo. Las clases "Estudiante" y "Profesor" heredan de la clase "Persona" y agregan atributos y métodos específicos para estudiantes y profesores, respectivamente.

El código también crea objetos de cada clase y los imprime en la consola. Esto muestra cómo se pueden usar las clases para crear objetos que representen entidades del mundo real y cómo se pueden usar las jerarquías de clases para organizar y reutilizar código.