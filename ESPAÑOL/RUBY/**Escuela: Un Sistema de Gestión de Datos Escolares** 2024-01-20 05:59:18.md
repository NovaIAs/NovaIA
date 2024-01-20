```ruby
# Definición de la clase Persona
class Persona
  # Atributos de la clase Persona
  attr_accessor :nombre, :edad, :sexo

  # Método de inicialización de la clase Persona
  def initialize(nombre, edad, sexo)
    @nombre = nombre
    @edad = edad
    @sexo = sexo
  end

  # Método para obtener el nombre de la persona
  def obtener_nombre
    @nombre
  end

  # Método para obtener la edad de la persona
  def obtener_edad
    @edad
  end

  # Método para obtener el sexo de la persona
  def obtener_sexo
    @sexo
  end
end

# Definición de la clase Alumno
class Alumno < Persona
  # Atributos de la clase Alumno
  attr_accessor :matricula, :promedio

  # Método de inicialización de la clase Alumno
  def initialize(nombre, edad, sexo, matricula, promedio)
    super(nombre, edad, sexo)
    @matricula = matricula
    @promedio = promedio
  end

  # Método para obtener la matrícula del alumno
  def obtener_matricula
    @matricula
  end

  # Método para obtener el promedio del alumno
  def obtener_promedio
    @promedio
  end
end

# Definición de la clase Profesor
class Profesor < Persona
  # Atributos de la clase Profesor
  attr_accessor :departamento, :sueldo

  # Método de inicialización de la clase Profesor
  def initialize(nombre, edad, sexo, departamento, sueldo)
    super(nombre, edad, sexo)
    @departamento = departamento
    @sueldo = sueldo
  end

  # Método para obtener el departamento del profesor
  def obtener_departamento
    @departamento
  end

  # Método para obtener el sueldo del profesor
  def obtener_sueldo
    @sueldo
  end
end

# Definición de la clase Escuela
class Escuela
  # Atributos de la clase Escuela
  attr_accessor :nombre, :direccion, :alumnos, :profesores

  # Método de inicialización de la clase Escuela
  def initialize(nombre, direccion)
    @nombre = nombre
    @direccion = direccion
    @alumnos = []
    @profesores = []
  end

  # Método para agregar un alumno a la escuela
  def agregar_alumno(alumno)
    @alumnos << alumno
  end

  # Método para agregar un profesor a la escuela
  def agregar_profesor(profesor)
    @profesores << profesor
  end

  # Método para obtener el nombre de la escuela
  def obtener_nombre
    @nombre
  end

  # Método para obtener la dirección de la escuela
  def obtener_direccion
    @direccion
  end

  # Método para obtener la lista de alumnos de la escuela
  def obtener_alumnos
    @alumnos
  end

  # Método para obtener la lista de profesores de la escuela
  def obtener_profesores
    @profesores
  end
end

# Creación de una escuela
escuela = Escuela.new("Escuela Politécnica Superior", "Avenida de la Politécnica, 1")

# Creación de algunos alumnos
alumno1 = Alumno.new("Juan", 20, "Masculino", "2020-01-01", 8.5)
alumno2 = Alumno.new("María", 21, "Femenino", "2021-01-01", 9.0)

# Creación de algunos profesores
profesor1 = Profesor.new("Pedro", 50, "Masculino", "Ingeniería Informática", 3000)
profesor2 = Profesor.new("Ana", 45, "Femenino", "Matemáticas", 2500)

# Adición de los alumnos y profesores a la escuela
escuela.agregar_alumno(alumno1)
escuela.agregar_alumno(alumno2)
escuela.agregar_profesor(profesor1)
escuela.agregar_profesor(profesor2)

# Impresión del nombre y la dirección de la escuela
puts "Nombre de la escuela: #{escuela.obtener_nombre}"
puts "Dirección de la escuela: #{escuela.obtener_direccion}"

# Impresión de la lista de alumnos de la escuela
puts "Alumnos de la escuela:"
escuela.obtener_alumnos.each do |alumno|
  puts "- #{alumno.obtener_nombre} (#{alumno.obtener_edad} años, #{alumno.obtener_sexo}, matrícula: #{alumno.obtener_matricula}, promedio: #{alumno.obtener_promedio})"
end

# Impresión de la lista de profesores de la escuela
puts "Profesores de la escuela:"
escuela.obtener_profesores.each do |profesor|
  puts "- #{profesor.obtener_nombre} (#{profesor.obtener_edad} años, #{profesor.obtener_sexo}, departamento: #{profesor.obtener_departamento}, sueldo: #{profesor.obtener_sueldo})"
end
```

**Explicación del código:**

* La primera parte del código define las clases `Persona`, `Alumno`, `Profesor` y `Escuela`. Estas clases representan los diferentes tipos de entidades que pueden existir en una escuela.
* La clase `Persona` es la clase base para todas las demás clases. Define los atributos comunes a todas las personas, como el nombre, la edad y el sexo.
* Las clases `Alumno` y `Profesor` heredan de la clase `Persona`. Estas clases añaden atributos específicos para cada tipo de entidad. Por ejemplo, la clase `Alumno` tiene atributos como la matrícula y el promedio, mientras que la clase `Profesor` tiene atributos como el departamento y el sueldo.
* La clase `Escuela` representa la escuela en sí. Tiene atributos como el nombre, la dirección, la lista de alumnos y la lista de profesores.
* La segunda parte del código crea una escuela, algunos alumnos y algunos profesores.
* La tercera parte del código agrega los alumnos y profesores a la escuela.
* La cuarta parte del código imprime el nombre y la dirección de la escuela, así como la lista de alumnos y profesores.

Este código es un ejemplo de cómo utilizar la herencia y la composición en la programación orientada a objetos para crear un modelo de datos complejo.