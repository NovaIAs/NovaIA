```ruby
# Este código crea una clase llamada "Persona" que tiene dos atributos: "nombre" y "edad".
class Persona
  # Este método inicializa los atributos de la clase "Persona".
  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  # Este método devuelve el nombre de la persona.
  def obtener_nombre
    @nombre
  end

  # Este método devuelve la edad de la persona.
  def obtener_edad
    @edad
  end

  # Este método aumenta la edad de la persona en un año.
  def envejecer
    @edad += 1
  end
end

# Este código crea una clase llamada "Estudiante" que hereda de la clase "Persona".
class Estudiante < Persona
  # Este método inicializa los atributos de la clase "Estudiante".
  def initialize(nombre, edad, matricula)
    super(nombre, edad)
    @matricula = matricula
  end

  # Este método devuelve la matrícula del estudiante.
  def obtener_matricula
    @matricula
  end
end

# Este código crea una clase llamada "Profesor" que hereda de la clase "Persona".
class Profesor < Persona
  # Este método inicializa los atributos de la clase "Profesor".
  def initialize(nombre, edad, materia)
    super(nombre, edad)
    @materia = materia
  end

  # Este método devuelve la materia que enseña el profesor.
  def obtener_materia
    @materia
  end
end

# Este código crea una lista de personas.
personas = [
  Persona.new("Juan", 20),
  Persona.new("María", 25),
  Estudiante.new("Pedro", 18, "123456"),
  Estudiante.new("Ana", 19, "654321"),
  Profesor.new("Luis", 30, "Matemáticas"),
  Profesor.new("Sara", 35, "Inglés")
]

# Este código imprime el nombre y la edad de cada persona en la lista.
personas.each do |persona|
  puts "Nombre: #{persona.obtener_nombre}, Edad: #{persona.obtener_edad}"
end

# Este código imprime la matrícula de cada estudiante en la lista.
estudiantes = personas.select { |persona| persona.is_a?(Estudiante) }
estudiantes.each do |estudiante|
  puts "Matrícula: #{estudiante.obtener_matricula}"
end

# Este código imprime la materia que enseña cada profesor en la lista.
profesores = personas.select { |persona| persona.is_a?(Profesor) }
profesores.each do |profesor|
  puts "Materia: #{profesor.obtener_materia}"
end
```

Explicación del código:

* La clase "Persona" tiene dos atributos: "nombre" y "edad". Estos atributos se inicializan en el método "initialize".
* La clase "Estudiante" hereda de la clase "Persona". Esto significa que la clase "Estudiante" tiene todos los atributos y métodos de la clase "Persona". Además, la clase "Estudiante" tiene un atributo adicional llamado "matricula".
* La clase "Profesor" también hereda de la clase "Persona". La clase "Profesor" tiene un atributo adicional llamado "materia".
* La variable "personas" es una lista de personas. Esta lista contiene objetos de las clases "Persona", "Estudiante" y "Profesor".
* El código imprime el nombre y la edad de cada persona en la lista.
* El código imprime la matrícula de cada estudiante en la lista.
* El código imprime la materia que enseña cada profesor en la lista.