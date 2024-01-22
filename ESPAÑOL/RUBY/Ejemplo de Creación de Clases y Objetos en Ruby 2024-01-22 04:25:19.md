```ruby
# Crear una clase Persona con atributos nombre, edad y género.

class Persona

  # Constructor de la clase Persona.
  # @param nombre [String] El nombre de la persona.
  # @param edad [Integer] La edad de la persona.
  # @param género [String] El género de la persona.
  def initialize(nombre, edad, género)
    @nombre = nombre
    @edad = edad
    @género = género
  end

  # Método para obtener el nombre de la persona.
  # @return [String] El nombre de la persona.
  def nombre
    @nombre
  end

  # Método para obtener la edad de la persona.
  # @return [Integer] La edad de la persona.
  def edad
    @edad
  end

  # Método para obtener el género de la persona.
  # @return [String] El género de la persona.
  def género
    @género
  end

  # Método para imprimir la información de la persona.
  # @return [String] La información de la persona.
  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Género: #{@género}"
  end
end

# Crear una clase Estudiante que hereda de la clase Persona.

class Estudiante < Persona

  # Constructor de la clase Estudiante.
  # @param nombre [String] El nombre del estudiante.
  # @param edad [Integer] La edad del estudiante.
  # @param género [String] El género del estudiante.
  # @param matrícula [String] La matrícula del estudiante.
  def initialize(nombre, edad, género, matrícula)
    super(nombre, edad, género)
    @matrícula = matrícula
  end

  # Método para obtener la matrícula del estudiante.
  # @return [String] La matrícula del estudiante.
  def matrícula
    @matrícula
  end

  # Método para imprimir la información del estudiante.
  # @return [String] La información del estudiante.
  def to_s
    super + ", Matrícula: #{@matrícula}"
  end
end

# Crear una clase Profesor que hereda de la clase Persona.

class Profesor < Persona

  # Constructor de la clase Profesor.
  # @param nombre [String] El nombre del profesor.
  # @param edad [Integer] La edad del profesor.
  # @param género [String] El género del profesor.
  # @param asignatura [String] La asignatura que imparte el profesor.
  def initialize(nombre, edad, género, asignatura)
    super(nombre, edad, género)
    @asignatura = asignatura
  end

  # Método para obtener la asignatura que imparte el profesor.
  # @return [String] La asignatura que imparte el profesor.
  def asignatura
    @asignatura
  end

  # Método para imprimir la información del profesor.
  # @return [String] La información del profesor.
  def to_s
    super + ", Asignatura: #{@asignatura}"
  end
end

# Crear una clase Curso que contiene una lista de estudiantes y profesores.

class Curso

  # Constructor de la clase Curso.
  # @param nombre [String] El nombre del curso.
  # @param estudiantes [Array<Estudiante>] La lista de estudiantes del curso.
  # @param profesores [Array<Profesor>] La lista de profesores del curso.
  def initialize(nombre, estudiantes, profesores)
    @nombre = nombre
    @estudiantes = estudiantes
    @profesores = profesores
  end

  # Método para obtener el nombre del curso.
  # @return [String] El nombre del curso.
  def nombre
    @nombre
  end

  # Método para obtener la lista de estudiantes del curso.
  # @return [Array<Estudiante>] La lista de estudiantes del curso.
  def estudiantes
    @estudiantes
  end

  # Método para obtener la lista de profesores del curso.
  # @return [Array<Profesor>] La lista de profesores del curso.
  def profesores
    @profesores
  end

  # Método para imprimir la información del curso.
  # @return [String] La información del curso.
  def to_s
    "Nombre: #{@nombre}, Estudiantes: #{@estudiantes.join(", ")}, Profesores: #{@profesores.join(", ")}"
  end
end

# Crear una instancia de la clase Persona.

persona1 = Persona.new("Juan", 20, "Masculino")

# Crear una instancia de la clase Estudiante.

estudiante1 = Estudiante.new("María", 18, "Femenino", "123456789")

# Crear una instancia de la clase Profesor.

profesor1 = Profesor.new("Pedro", 40, "Masculino", "Matemáticas")

# Crear una instancia de la clase Curso.

curso1 = Curso.new("Introducción a la Programación", [estudiante1], [profesor1])

# Imprimir la información de la persona.

puts persona1

# Imprimir la información del estudiante.

puts estudiante1

# Imprimir la información del profesor.

puts profesor1

# Imprimir la información del curso.

puts curso1
```

Este código es un ejemplo de cómo crear clases y objetos en Ruby.

La clase `Persona` es una clase base que define los atributos y métodos comunes a todas las personas. La clase `Estudiante` es una clase derivada de la clase `Persona` que define los atributos y métodos específicos de los estudiantes. La clase `Profesor` es otra clase derivada de la clase `Persona` que define los atributos y métodos específicos de los profesores.

La clase `Curso` es una clase que contiene una lista de estudiantes y profesores.

El código crea una instancia de la clase `Persona`, una instancia de la clase `Estudiante`, una instancia de la clase `Profesor` y una instancia de la clase `Curso`. Luego, imprime la información de cada una de las instancias.