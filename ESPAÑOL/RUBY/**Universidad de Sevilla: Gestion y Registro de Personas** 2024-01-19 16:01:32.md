```ruby
# Clase Persona que define los atributos y métodos básicos de una persona
class Persona
  attr_accessor :nombre, :edad, :genero

  def initialize(nombre, edad, genero)
    @nombre = nombre
    @edad = edad
    @genero = genero
  end

  def saludar
    "Hola, mi nombre es #{nombre} y tengo #{edad} años."
  end
end

# Clase Estudiante que hereda de la clase Persona y añade atributos y métodos específicos de un estudiante
class Estudiante < Persona
  attr_accessor :matricula, :carrera

  def initialize(nombre, edad, genero, matricula, carrera)
    super(nombre, edad, genero)
    @matricula = matricula
    @carrera = carrera
  end

  def estudiar
    "Estoy estudiando #{carrera} en la universidad."
  end
end

# Clase Profesor que hereda de la clase Persona y añade atributos y métodos específicos de un profesor
class Profesor < Persona
  attr_accessor :asignatura, :departamento

  def initialize(nombre, edad, genero, asignatura, departamento)
    super(nombre, edad, genero)
    @asignatura = asignatura
    @departamento = departamento
  end

  def enseñar
    "Estoy enseñando #{asignatura} en el departamento de #{departamento}."
  end
end

# Clase Universidad que agrupa a personas, estudiantes y profesores
class Universidad
  attr_accessor :nombre, :facultades, :estudiantes, :profesores

  def initialize(nombre)
    @nombre = nombre
    @facultades = []
    @estudiantes = []
    @profesores = []
  end

  def añadir_facultad(facultad)
    @facultades << facultad
  end

  def añadir_estudiante(estudiante)
    @estudiantes << estudiante
  end

  def añadir_profesor(profesor)
    @profesores << profesor
  end

  def listar_facultades
    @facultades.each do |facultad|
      puts facultad.nombre
    end
  end

  def listar_estudiantes
    @estudiantes.each do |estudiante|
      puts estudiante.nombre
    end
  end

  def listar_profesores
    @profesores.each do |profesor|
      puts profesor.nombre
    end
  end
end

# Creamos una universidad llamada "Universidad de Sevilla"
universidad = Universidad.new("Universidad de Sevilla")

# Creamos las facultades de la universidad
facultad_ciencias = Facultad.new("Ciencias")
facultad_ingenieria = Facultad.new("Ingeniería")
facultad_humanidades = Facultad.new("Humanidades")

# Añadimos las facultades a la universidad
universidad.añadir_facultad(facultad_ciencias)
universidad.añadir_facultad(facultad_ingenieria)
universidad.añadir_facultad(facultad_humanidades)

# Creamos algunos estudiantes y profesores
estudiante1 = Estudiante.new("Juan", 20, "masculino", "123456", "Ingeniería Informática")
estudiante2 = Estudiante.new("María", 21, "femenino", "654321", "Ciencias Biológicas")
profesor1 = Profesor.new("Pedro", 45, "masculino", "Matemáticas", "Ciencias")
profesor2 = Profesor.new("Ana", 50, "femenino", "Lengua Española", "Humanidades")

# Añadimos los estudiantes y profesores a la universidad
universidad.añadir_estudiante(estudiante1)
universidad.añadir_estudiante(estudiante2)
universidad.añadir_profesor(profesor1)
universidad.añadir_profesor(profesor2)

# Listamos las facultades, estudiantes y profesores de la universidad
puts "Facultades:"
universidad.listar_facultades
puts "Estudiantes:"
universidad.listar_estudiantes
puts "Profesores:"
universidad.listar_profesores
```

Este código es un ejemplo bastante complejo de código en Ruby que implementa un sistema universitario. El código define las clases Persona, Estudiante, Profesor y Universidad. Estas clases tienen atributos y métodos específicos para cada tipo de persona. Por ejemplo, la clase Persona tiene atributos para el nombre, la edad y el género, y métodos para saludar. La clase Estudiante hereda de la clase Persona y añade atributos y métodos específicos para un estudiante, como la matrícula y la carrera. La clase Profesor también hereda de la clase Persona y añade atributos y métodos específicos para un profesor, como la asignatura y el departamento. La clase Universidad agrupa a personas, estudiantes y profesores, y tiene métodos para añadir y listar estos elementos.

El código también crea una universidad llamada "Universidad de Sevilla" y añade las facultades de Ciencias, Ingeniería y Humanidades. A continuación, crea algunos estudiantes y profesores y los añade a la universidad. Por último, el código lista las facultades, estudiantes y profesores de la universidad.

Este código es sólo un ejemplo y se puede ampliar y modificar para crear un sistema universitario más complejo. Por ejemplo, se podrían añadir clases para los cursos, los horarios y las notas. También se podrían añadir métodos para generar informes y estadísticas.