```ruby
# Definir una clase llamada "Persona" con atributos como nombre, edad y género.
class Persona
  attr_accessor :nombre, :edad, :genero

  # Método para inicializar los atributos de una persona.
  def initialize(nombre, edad, genero)
    @nombre = nombre
    @edad = edad
    @genero = genero
  end

  # Método para imprimir la información de una persona.
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Género: #{@genero}"
  end
end

# Definir una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante < Persona
  attr_accessor :matricula, :carrera

  # Método para inicializar los atributos de un estudiante.
  def initialize(nombre, edad, genero, matricula, carrera)
    super(nombre, edad, genero)
    @matricula = matricula
    @carrera = carrera
  end

  # Método para imprimir la información de un estudiante.
  def imprimir_informacion
    super
    puts "Matrícula: #{@matricula}"
    puts "Carrera: #{@carrera}"
  end
end

# Definir una clase llamada "Profesor" que hereda de la clase "Persona".
class Profesor < Persona
  attr_accessor :departamento, :asignatura

  # Método para inicializar los atributos de un profesor.
  def initialize(nombre, edad, genero, departamento, asignatura)
    super(nombre, edad, genero)
    @departamento = departamento
    @asignatura = asignatura
  end

  # Método para imprimir la información de un profesor.
  def imprimir_informacion
    super
    puts "Departamento: #{@departamento}"
    puts "Asignatura: #{@asignatura}"
  end
end

# Crear un objeto de la clase "Persona" llamado "persona1".
persona1 = Persona.new("Juan", 25, "Masculino")

# Crear un objeto de la clase "Estudiante" llamado "estudiante1".
estudiante1 = Estudiante.new("María", 20, "Femenino", "123456", "Ingeniería en Informática")

# Crear un objeto de la clase "Profesor" llamado "profesor1".
profesor1 = Profesor.new("Pedro", 40, "Masculino", "Departamento de Matemáticas", "Matemáticas")

# Imprimir la información de los objetos creados.
persona1.imprimir_informacion
puts
estudiante1.imprimir_informacion
puts
profesor1.imprimir_informacion
```

Este código define tres clases: "Persona", "Estudiante" y "Profesor". La clase "Persona" tiene tres atributos: "nombre", "edad" y "genero". La clase "Estudiante" hereda de la clase "Persona" y tiene dos atributos adicionales: "matricula" y "carrera". La clase "Profesor" también hereda de la clase "Persona" y tiene dos atributos adicionales: "departamento" y "asignatura".

El código también crea tres objetos: "persona1", "estudiante1" y "profesor1". El objeto "persona1" es una instancia de la clase "Persona". El objeto "estudiante1" es una instancia de la clase "Estudiante". El objeto "profesor1" es una instancia de la clase "Profesor".

Finalmente, el código imprime la información de los objetos creados.

Este código es complejo porque:

* Define tres clases con diferentes atributos y métodos.
* Crea tres objetos de diferentes clases.
* Utiliza la herencia para crear la clase "Estudiante" y la clase "Profesor".
* Imprime la información de los objetos creados de una manera ordenada y clara.