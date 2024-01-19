```ruby
# Crear una clase llamada "Persona" con atributos "nombre", "apellido" y "edad"
class Persona
  attr_accessor :nombre, :apellido, :edad

  # Constructor de la clase "Persona" que recibe los valores de los atributos
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

# Crear una clase llamada "Estudiante" que hereda de la clase "Persona" y agrega el atributo "calificaciones"
class Estudiante < Persona
  attr_accessor :calificaciones

  # Constructor de la clase "Estudiante" que recibe los valores de los atributos
  def initialize(nombre, apellido, edad, calificaciones)
    super(nombre, apellido, edad)
    @calificaciones = calificaciones
  end

  # Método que devuelve el promedio de las calificaciones del estudiante
  def promedio_calificaciones
    @calificaciones.sum / @calificaciones.size
  end
end

# Crear una clase llamada "Profesor" que hereda de la clase "Persona" y agrega el atributo "materias"
class Profesor < Persona
  attr_accessor :materias

  # Constructor de la clase "Profesor" que recibe los valores de los atributos
  def initialize(nombre, apellido, edad, materias)
    super(nombre, apellido, edad)
    @materias = materias
  end

  # Método que devuelve las materias que imparte el profesor
  def materias_impartidas
    @materias.join(", ")
  end
end

# Crear una clase llamada "Escuela" que tiene un atributo "alumnos" y un atributo "profesores"
class Escuela
  attr_accessor :alumnos, :profesores

  # Constructor de la clase "Escuela" que recibe los valores de los atributos
  def initialize(alumnos, profesores)
    @alumnos = alumnos
    @profesores = profesores
  end

  # Método que devuelve la lista de alumnos de la escuela
  def lista_alumnos
    @alumnos.map { |alumno| alumno.nombre_completo }.join("\n")
  end

  # Método que devuelve la lista de profesores de la escuela
  def lista_profesores
    @profesores.map { |profesor| profesor.nombre_completo }.join("\n")
  end

  # Método que devuelve el promedio de calificaciones de los alumnos de la escuela
  def promedio_calificaciones
    @alumnos.map { |alumno| alumno.promedio_calificaciones }.sum / @alumnos.size
  end
end

# Crear una instancia de la clase "Escuela" con los alumnos y profesores correspondientes
escuela = Escuela.new(
  [
    Estudiante.new("Juan", "Pérez", 18, [9, 8, 7]),
    Estudiante.new("María", "González", 19, [10, 9, 8]),
    Estudiante.new("Pedro", "Rodríguez", 20, [7, 6, 5])
  ],
  [
    Profesor.new("Ana", "López", 40, ["Matemáticas", "Ciencias"]),
    Profesor.new("Luis", "Fernández", 45, ["Lengua", "Historia"])
  ]
)

# Imprimir la lista de alumnos de la escuela
puts "Alumnos:"
puts escuela.lista_alumnos

# Imprimir la lista de profesores de la escuela
puts "Profesores:"
puts escuela.lista_profesores

# Imprimir el promedio de calificaciones de los alumnos de la escuela
puts "Promedio de calificaciones:"
puts escuela.promedio_calificaciones
```

Este código es un ejemplo de programación orientada a objetos en Ruby. Crea varias clases, incluyendo "Persona", "Estudiante", "Profesor" y "Escuela", y las utiliza para crear objetos que representan alumnos, profesores y una escuela. También incluye métodos para manipular y mostrar los datos de estos objetos.