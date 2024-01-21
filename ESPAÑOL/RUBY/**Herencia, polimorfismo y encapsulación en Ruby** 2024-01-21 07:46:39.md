```ruby
# Clase Persona
class Persona
  # Atributos
  attr_reader :nombre, :edad, :ocupacion

  # Constructor
  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  # Método para saludar
  def saludar
    puts "Hola, mi nombre es #{nombre} y tengo #{edad} años. Soy #{ocupacion}."
  end
end

# Clase Estudiante
class Estudiante < Persona
  # Atributos
  attr_reader :calificacion

  # Constructor
  def initialize(nombre, edad, ocupacion, calificacion)
    super(nombre, edad, ocupacion)
    @calificacion = calificacion
  end

  # Método para estudiar
  def estudiar
    puts "Estoy estudiando para obtener una buena calificación."
  end
end

# Clase Profesor
class Profesor < Persona
  # Atributos
  attr_reader :materia

  # Constructor
  def initialize(nombre, edad, ocupacion, materia)
    super(nombre, edad, ocupacion)
    @materia = materia
  end

  # Método para enseñar
  def enseñar
    puts "Estoy enseñando #{materia} a mis estudiantes."
  end
end

# Clase Escuela
class Escuela
  # Atributos
  attr_reader :nombre, :direccion, :estudiantes, :profesores

  # Constructor
  def initialize(nombre, direccion, estudiantes, profesores)
    @nombre = nombre
    @direccion = direccion
    @estudiantes = estudiantes
    @profesores = profesores
  end

  # Método para agregar un estudiante
  def agregar_estudiante(estudiante)
    @estudiantes << estudiante
  end

  # Método para agregar un profesor
  def agregar_profesor(profesor)
    @profesores << profesor
  end

  # Método para mostrar la información de la escuela
  def mostrar_informacion
    puts "Nombre de la escuela: #{nombre}"
    puts "Dirección de la escuela: #{direccion}"
    puts "Estudiantes de la escuela:"
    @estudiantes.each do |estudiante|
      puts "- #{estudiante.nombre} (#{estudiante.edad} años, #{estudiante.ocupacion}, calificación: #{estudiante.calificacion})"
    end
    puts "Profesores de la escuela:"
    @profesores.each do |profesor|
      puts "- #{profesor.nombre} (#{profesor.edad} años, #{profesor.ocupacion}, materia: #{profesor.materia})"
    end
  end
end

# Crear objetos
juan = Estudiante.new("Juan", 18, "Estudiante", 9.5)
maria = Estudiante.new("María", 19, "Estudiante", 9.0)
pedro = Profesor.new("Pedro", 35, "Profesor", "Matemáticas")
ana = Profesor.new("Ana", 40, "Profesora", "Lengua")

# Crear una escuela
escuela = Escuela.new("Instituto Nacional", "Calle Mayor, 123", [juan, maria], [pedro, ana])

# Mostrar la información de la escuela
escuela.mostrar_informacion
```

Explicación del código:

* Se define una clase `Persona` con atributos para el nombre, la edad y la ocupación.
* Se definen dos clases hijas, `Estudiante` y `Profesor`, que heredan de la clase `Persona`.
* Ambas clases hijas añaden atributos específicos, como la calificación para el estudiante y la materia para el profesor.
* Se definen métodos para saludar, estudiar y enseñar en las clases `Persona`, `Estudiante` y `Profesor` respectivamente.
* Se define una clase `Escuela` con atributos para el nombre, la dirección, los estudiantes y los profesores.
* Se definen métodos para agregar estudiantes y profesores, y para mostrar la información de la escuela.
* Se crean objetos de las clases `Estudiante`, `Profesor` y `Escuela`.
* Se añaden los estudiantes y profesores a la escuela.
* Se muestra la información de la escuela.

Este código es bastante complejo, ya que utiliza herencia, polimorfismo y encapsulación. También es bastante amplio, ya que cubre una amplia gama de conceptos y funcionalidades. Es poco probable que se repita exactamente el mismo código, ya que cada aplicación o proyecto tiene sus propios requisitos específicos.