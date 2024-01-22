```ruby
# Creación de una clase Persona
class Persona
  attr_accessor :nombre, :apellido, :edad

  # Constructor de la clase Persona
  def initialize(nombre, apellido, edad)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
  end

  # Método que devuelve el nombre completo de la persona
  def nombre_completo
    "#{@nombre} #{@apellido}"
  end

  # Método que devuelve la edad de la persona
  def edad
    @edad
  end
end

# Creación de una clase Estudiante que hereda de la clase Persona
class Estudiante < Persona
  attr_accessor :matricula, :calificaciones

  # Constructor de la clase Estudiante
  def initialize(nombre, apellido, edad, matricula, calificaciones)
    super(nombre, apellido, edad)
    @matricula = matricula
    @calificaciones = calificaciones
  end

  # Método que devuelve el promedio de las calificaciones del estudiante
  def promedio
    @calificaciones.sum / @calificaciones.size
  end
end

# Creación de una clase Profesor que hereda de la clase Persona
class Profesor < Persona
  attr_accessor :asignatura, :salario

  # Constructor de la clase Profesor
  def initialize(nombre, apellido, edad, asignatura, salario)
    super(nombre, apellido, edad)
    @asignatura = asignatura
    @salario = salario
  end

  # Método que devuelve la asignatura que imparte el profesor
  def asignatura
    @asignatura
  end

  # Método que devuelve el salario del profesor
  def salario
    @salario
  end
end

# Creación de objetos de las clases Persona, Estudiante y Profesor
persona1 = Persona.new("Juan", "García", 30)
estudiante1 = Estudiante.new("María", "Pérez", 18, "12345", [8, 9, 10])
profesor1 = Profesor.new("Pedro", "López", 45, "Matemáticas", 2000)

# Impresión de los datos de los objetos
puts "Nombre de la persona: #{persona1.nombre_completo}"
puts "Edad de la persona: #{persona1.edad}"
puts "Nombre del estudiante: #{estudiante1.nombre_completo}"
puts "Edad del estudiante: #{estudiante1.edad}"
puts "Matrícula del estudiante: #{estudiante1.matricula}"
puts "Calificaciones del estudiante: #{estudiante1.calificaciones}"
puts "Promedio del estudiante: #{estudiante1.promedio}"
puts "Nombre del profesor: #{profesor1.nombre_completo}"
puts "Edad del profesor: #{profesor1.edad}"
puts "Asignatura del profesor: #{profesor1.asignatura}"
puts "Salario del profesor: #{profesor1.salario}"
```

Este código crea tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` es una clase base que define los atributos y métodos comunes a todas las personas. La clase `Estudiante` hereda de la clase `Persona` y define atributos y métodos específicos de los estudiantes. La clase `Profesor` también hereda de la clase `Persona` y define atributos y métodos específicos de los profesores.

A continuación, se crean objetos de las tres clases y se imprimen sus datos. El código utiliza el método `puts` para imprimir los datos de los objetos. El método `puts` imprime los argumentos que se le pasan separados por un espacio y un salto de línea.

Este código es complejo porque utiliza varias clases, herencia, objetos y métodos. Sin embargo, es un código bien escrito y fácil de entender.