```ruby
# Definir una clase llamada "Persona" con atributos "nombre" y "edad"
class Persona
  attr_accessor :nombre, :edad

  # Constructor de la clase "Persona"
  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  # Método para obtener el nombre de la persona
  def obtener_nombre
    @nombre
  end

  # Método para obtener la edad de la persona
  def obtener_edad
    @edad
  end

  # Método para saludar a la persona
  def saludar
    "Hola, mi nombre es #{@nombre} y tengo #{@edad} años."
  end
end

# Definir una clase llamada "Estudiante" que hereda de la clase "Persona"
class Estudiante < Persona
  attr_accessor :calificaciones

  # Constructor de la clase "Estudiante"
  def initialize(nombre, edad, calificaciones)
    super(nombre, edad)
    @calificaciones = calificaciones
  end

  # Método para obtener el promedio de las calificaciones del estudiante
  def obtener_promedio
    @calificaciones.inject(0) { |sum, cal| sum + cal } / @calificaciones.size
  end

  # Método para imprimir el nombre del estudiante y su promedio de calificaciones
  def imprimir_nombre_y_promedio
    "El estudiante #{@nombre} tiene un promedio de calificaciones de #{obtener_promedio}."
  end
end

# Definir una clase llamada "Profesor" que hereda de la clase "Persona"
class Profesor < Persona
  attr_accessor :asignaturas

  # Constructor de la clase "Profesor"
  def initialize(nombre, edad, asignaturas)
    super(nombre, edad)
    @asignaturas = asignaturas
  end

  # Método para imprimir el nombre del profesor y las asignaturas que imparte
  def imprimir_nombre_y_asignaturas
    "El profesor #{@nombre} imparte las siguientes asignaturas: #{@asignaturas.join(', ')}."
  end
end

# Crear un objeto de la clase "Persona"
persona = Persona.new("Juan", 30)

# Imprimir el nombre de la persona
puts persona.obtener_nombre

# Crear un objeto de la clase "Estudiante"
estudiante = Estudiante.new("María", 20, [8, 9, 7])

# Imprimir el nombre del estudiante y su promedio de calificaciones
puts estudiante.imprimir_nombre_y_promedio

# Crear un objeto de la clase "Profesor"
profesor = Profesor.new("Luis", 40, ["Matemáticas", "Física", "Química"])

# Imprimir el nombre del profesor y las asignaturas que imparte
puts profesor.imprimir_nombre_y_asignaturas
```

Este código define tres clases en Ruby: "Persona", "Estudiante" y "Profesor". La clase "Persona" tiene dos atributos: "nombre" y "edad". La clase "Estudiante" hereda de la clase "Persona" y tiene un atributo adicional: "calificaciones". La clase "Profesor" también hereda de la clase "Persona" y tiene un atributo adicional: "asignaturas".

El código crea un objeto de la clase "Persona", un objeto de la clase "Estudiante" y un objeto de la clase "Profesor". Luego, imprime el nombre de la persona, el nombre del estudiante y su promedio de calificaciones, y el nombre del profesor y las asignaturas que imparte.

Este código es complejo porque utiliza herencia de clases, métodos y atributos dinámicos, y también porque utiliza el método `inject` para calcular el promedio de las calificaciones del estudiante.