```ruby
# **Creación de una clase Persona**
class Persona
  # **Definición de los atributos de la clase Persona**
  attr_accessor :nombre, :edad, :ciudad

  # **Método constructor de la clase Persona**
  def initialize(nombre, edad, ciudad)
    @nombre = nombre
    @edad = edad
    @ciudad = ciudad
  end

  # **Método para obtener la información de la persona**
  def info
    "Nombre: #{@nombre}, Edad: #{@edad}, Ciudad: #{@ciudad}"
  end
end

# **Creación de una clase Estudiante que hereda de la clase Persona**
class Estudiante < Persona
  # **Definición de los atributos de la clase Estudiante**
  attr_accessor :curso, :promedio

  # **Método constructor de la clase Estudiante**
  def initialize(nombre, edad, ciudad, curso, promedio)
    super(nombre, edad, ciudad)
    @curso = curso
    @promedio = promedio
  end

  # **Método para obtener la información del estudiante**
  def info
    "#{super}, Curso: #{@curso}, Promedio: #{@promedio}"
  end
end

# **Creación de una clase Profesor que hereda de la clase Persona**
class Profesor < Persona
  # **Definición de los atributos de la clase Profesor**
  attr_accessor :asignatura, :sueldo

  # **Método constructor de la clase Profesor**
  def initialize(nombre, edad, ciudad, asignatura, sueldo)
    super(nombre, edad, ciudad)
    @asignatura = asignatura
    @sueldo = sueldo
  end

  # **Método para obtener la información del profesor**
  def info
    "#{super}, Asignatura: #{@asignatura}, Sueldo: #{@sueldo}"
  end
end

# **Creación de una instancia de la clase Persona**
persona = Persona.new("Juan", 25, "Madrid")

# **Obtención de la información de la persona**
puts persona.info

# **Creación de una instancia de la clase Estudiante**
estudiante = Estudiante.new("María", 20, "Barcelona", "Ingeniería Informática", 8.5)

# **Obtención de la información del estudiante**
puts estudiante.info

# **Creación de una instancia de la clase Profesor**
profesor = Profesor.new("Luis", 45, "Sevilla", "Matemáticas", 2500)

# **Obtención de la información del profesor**
puts profesor.info
```

**Explicación del código:**

* **Creación de la clase Persona:** Se crea una clase denominada `Persona` que representa a una persona genérica. La clase contiene tres atributos: `nombre`, `edad` y `ciudad`.

* **Método constructor de la clase Persona:** El método constructor de la clase `Persona` inicializa los atributos de la clase con los valores recibidos como parámetros.

* **Método `info` de la clase Persona:** El método `info` de la clase `Persona` devuelve una cadena de texto con la información de la persona.

* **Creación de la clase Estudiante:** Se crea una clase denominada `Estudiante` que hereda de la clase `Persona`. La clase `Estudiante` contiene dos atributos adicionales: `curso` y `promedio`.

* **Método constructor de la clase Estudiante:** El método constructor de la clase `Estudiante` inicializa los atributos de la clase `Estudiante` con los valores recibidos como parámetros. El método también llama al método constructor de la clase `Persona` para inicializar los atributos heredados.

* **Método `info` de la clase Estudiante:** El método `info` de la clase `Estudiante` devuelve una cadena de texto con la información del estudiante, incluyendo el nombre, la edad, la ciudad, el curso y el promedio.

* **Creación de la clase Profesor:** Se crea una clase denominada `Profesor` que hereda de la clase `Persona`. La clase `Profesor` contiene dos atributos adicionales: `asignatura` y `sueldo`.

* **Método constructor de la clase Profesor:** El método constructor de la clase `Profesor` inicializa los atributos de la clase `Profesor` con los valores recibidos como parámetros. El método también llama al método constructor de la clase `Persona` para inicializar los atributos heredados.

* **Método `info` de la clase Profesor:** El método `info` de la clase `Profesor` devuelve una cadena de texto con la información del profesor, incluyendo el nombre, la edad, la ciudad, la asignatura y el sueldo.

* **Creación de instancias de las clases Persona, Estudiante y Profesor:** Se crean tres instancias de las clases `Persona`, `Estudiante` y `Profesor`.

* **Obtención de la información de las instancias:** Se llama al método `info` de cada instancia para obtener la información correspondiente a cada una. La información se muestra en la terminal.