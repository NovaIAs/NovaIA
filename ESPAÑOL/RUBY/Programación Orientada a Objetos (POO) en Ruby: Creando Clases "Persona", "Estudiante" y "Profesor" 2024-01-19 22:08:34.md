```ruby
# Definición de la clase "Persona"
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  def saludar
    puts "Hola, mi nombre es #{@nombre} y tengo #{@edad} años. Soy un #{@ocupacion}."
  end
end

# Definición de la clase "Estudiante"
class Estudiante < Persona
  attr_accessor :universidad, :carrera

  def initialize(nombre, edad, universidad, carrera)
    super(nombre, edad, "Estudiante")
    @universidad = universidad
    @carrera = carrera
  end

  def estudiar
    puts "Estoy estudiando #{@carrera} en la #{@universidad}."
  end
end

# Definición de la clase "Profesor"
class Profesor < Persona
  attr_accessor :universidad, :asignatura

  def initialize(nombre, edad, universidad, asignatura)
    super(nombre, edad, "Profesor")
    @universidad = universidad
    @asignatura = asignatura
  end

  def enseñar
    puts "Estoy enseñando #{@asignatura} en la #{@universidad}."
  end
end

# Creación de objetos "Persona", "Estudiante" y "Profesor"
persona1 = Persona.new("Juan", 25, "Ingeniero")
estudiante1 = Estudiante.new("María", 20, "Universidad de Sevilla", "Ingeniería Informática")
profesor1 = Profesor.new("Antonio", 45, "Universidad de Málaga", "Matemáticas")

# Invocación de los métodos de cada objeto
persona1.saludar
estudiante1.saludar
estudiante1.estudiar
profesor1.saludar
profesor1.enseñar
```

Explicación del código:

1. Se definen tres clases: "Persona", "Estudiante" y "Profesor". Cada clase tiene sus propios atributos y métodos.

2. La clase "Persona" es la clase base de las clases "Estudiante" y "Profesor". Esto significa que las clases "Estudiante" y "Profesor" heredan los atributos y métodos de la clase "Persona".

3. Se crean objetos de las clases "Persona", "Estudiante" y "Profesor".

4. Se invocan los métodos de cada objeto para mostrar los datos de los objetos y realizar las acciones correspondientes.

Este código es complejo porque utiliza múltiples clases, herencia, atributos y métodos. También utiliza el patrón de diseño "Polimorfismo", que permite que los objetos de diferentes clases respondan al mismo método de diferentes maneras.