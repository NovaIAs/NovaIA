**Código:**

```ruby
# Definición de la clase Persona
class Persona
  # Atributos de la clase
  attr_accessor :nombre, :edad, :sexo

  # Constructor de la clase
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

  # Método para imprimir la información de la persona
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Sexo: #{@sexo}"
  end
end

# Definición de la clase Estudiante
class Estudiante < Persona
  # Atributos de la clase
  attr_accessor :carrera, :promedio

  # Constructor de la clase
  def initialize(nombre, edad, sexo, carrera, promedio)
    super(nombre, edad, sexo)
    @carrera = carrera
    @promedio = promedio
  end

  # Método para obtener la carrera del estudiante
  def obtener_carrera
    @carrera
  end

  # Método para obtener el promedio del estudiante
  def obtener_promedio
    @promedio
  end

  # Método para imprimir la información del estudiante
  def imprimir_informacion
    super
    puts "Carrera: #{@carrera}"
    puts "Promedio: #{@promedio}"
  end
end

# Definición de la clase Profesor
class Profesor < Persona
  # Atributos de la clase
  attr_accessor :asignatura, :salario

  # Constructor de la clase
  def initialize(nombre, edad, sexo, asignatura, salario)
    super(nombre, edad, sexo)
    @asignatura = asignatura
    @salario = salario
  end

  # Método para obtener la asignatura del profesor
  def obtener_asignatura
    @asignatura
  end

  # Método para obtener el salario del profesor
  def obtener_salario
    @salario
  end

  # Método para imprimir la información del profesor
  def imprimir_informacion
    super
    puts "Asignatura: #{@asignatura}"
    puts "Salario: #{@salario}"
  end
end

# Definición de la clase Universidad
class Universidad
  # Atributos de la clase
  attr_accessor :nombre, :direccion, :estudiantes, :profesores

  # Constructor de la clase
  def initialize(nombre, direccion)
    @nombre = nombre
    @direccion = direccion
    @estudiantes = []
    @profesores = []
  end

  # Método para agregar un estudiante a la universidad
  def agregar_estudiante(estudiante)
    @estudiantes << estudiante
  end

  # Método para agregar un profesor a la universidad
  def agregar_profesor(profesor)
    @profesores << profesor
  end

  # Método para imprimir la información de la universidad
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Dirección: #{@direccion}"
    puts "Estudiantes:"
    @estudiantes.each do |estudiante|
      estudiante.imprimir_informacion
    end
    puts "Profesores:"
    @profesores.each do |profesor|
      profesor.imprimir_informacion
    end
  end
end

# Creación de objetos

# Creación de tres estudiantes
estudiante1 = Estudiante.new("Juan Pérez", 20, "Masculino", "Ingeniería de Sistemas", 3.8)
estudiante2 = Estudiante.new("María González", 22, "Femenino", "Administración de Empresas", 3.6)
estudiante3 = Estudiante.new("Pedro Rodríguez", 24, "Masculino", "Contabilidad", 3.4)

# Creación de tres profesores
profesor1 = Profesor.new("Carlos López", 40, "Masculino", "Matemáticas", 2000)
profesor2 = Profesor.new("Ana Jiménez", 35, "Femenino", "Español", 1800)
profesor3 = Profesor.new("Luis García", 45, "Masculino", "Física", 2200)

# Creación de una universidad
universidad = Universidad.new("Universidad Nacional", "Ciudad Universitaria")

# Adición de los estudiantes y profesores a la universidad
universidad.agregar_estudiante(estudiante1)
universidad.agregar_estudiante(estudiante2)
universidad.agregar_estudiante(estudiante3)
universidad.agregar_profesor(profesor1)
universidad.agregar_profesor(profesor2)
universidad.agregar_profesor(profesor3)

# Impresión de la información de la universidad
universidad.imprimir_informacion
```

**Explicación del código:**

El código anterior es un ejemplo de cómo crear clases y objetos en Ruby.

* **Clases:**

    * **Persona:** Esta clase define los atributos y métodos comunes a todas las personas.
    * **Estudiante:** Esta clase hereda de la clase Persona y añade atributos y métodos específicos de los estudiantes.
    * **Profesor:** Esta clase hereda de la clase Persona y añade atributos y métodos específicos de los profesores.
    * **Universidad:** Esta clase define los atributos y métodos comunes a todas las universidades.

* **Objetos:**

    * **Estudiante1, estudiante2, estudiante3:** Estos objetos son instancias de la clase Estudiante.
    * **Profesor1, profesor2, profesor3:** Estos objetos son instancias de la clase Profesor.
    * **Universidad:** Este objeto es una instancia de la clase Universidad.

El código anterior crea los objetos, los agrega a la universidad y luego imprime la información de la universidad.