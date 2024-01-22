```ruby
# Crear una clase llamada Persona con los atributos nombre, edad y ocupación
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  # Método para crear una nueva persona
  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  # Método para obtener el nombre de la persona
  def obtener_nombre
    @nombre
  end

  # Método para obtener la edad de la persona
  def obtener_edad
    @edad
  end

  # Método para obtener la ocupación de la persona
  def obtener_ocupacion
    @ocupacion
  end

  # Método para imprimir la información de la persona
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Ocupación: #{@ocupacion}"
  end
end

# Crear una clase llamada Estudiante que herede de la clase Persona y agregue el atributo calificacion
class Estudiante < Persona
  attr_accessor :calificacion

  # Método para crear un nuevo estudiante
  def initialize(nombre, edad, ocupacion, calificacion)
    super(nombre, edad, ocupacion)
    @calificacion = calificacion
  end

  # Método para obtener la calificación del estudiante
  def obtener_calificacion
    @calificacion
  end

  # Método para imprimir la información del estudiante
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Ocupación: #{@ocupacion}"
    puts "Calificación: #{@calificacion}"
  end
end

# Crear una clase llamada Profesor que herede de la clase Persona y agregue el atributo materias
class Profesor < Persona
  attr_accessor :materias

  # Método para crear un nuevo profesor
  def initialize(nombre, edad, ocupacion, materias)
    super(nombre, edad, ocupacion)
    @materias = materias
  end

  # Método para obtener las materias del profesor
  def obtener_materias
    @materias
  end

  # Método para imprimir la información del profesor
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Ocupación: #{@ocupacion}"
    puts "Materias: #{@materias}"
  end
end

# Crear una lista de estudiantes
estudiantes = [
  Estudiante.new("Juan", 20, "Estudiante", 90),
  Estudiante.new("María", 21, "Estudiante", 85),
  Estudiante.new("Pedro", 22, "Estudiante", 75)
]

# Crear una lista de profesores
profesores = [
  Profesor.new("Carlos", 40, "Profesor", ["Matemáticas", "Física"]),
  Profesor.new("Ana", 35, "Profesora", ["Lenguaje", "Literatura"]),
  Profesor.new("Luis", 45, "Profesor", ["Historia", "Geografía"])
]

# Imprimir la información de los estudiantes y profesores
puts "Estudiantes:"
estudiantes.each do |estudiante|
  estudiante.imprimir_informacion
  puts "------"
end

puts "Profesores:"
profesores.each do |profesor|
  profesor.imprimir_informacion
  puts "------"
end
```

Este código crea tres clases: `Persona`, `Estudiante` y `Profesor`. La clase `Persona` tiene los atributos `nombre`, `edad` y `ocupacion`. La clase `Estudiante` hereda de la clase `Persona` y agrega el atributo `calificacion`. La clase `Profesor` hereda de la clase `Persona` y agrega el atributo `materias`.

Luego, crea dos listas: una de estudiantes y otra de profesores. Cada elemento de la lista es una instancia de la clase `Estudiante` o `Profesor`.

Por último, el código imprime la información de los estudiantes y profesores.