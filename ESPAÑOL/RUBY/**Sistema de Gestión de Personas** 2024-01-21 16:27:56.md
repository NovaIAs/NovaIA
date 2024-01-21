```ruby
# Definición de clases

# Clase Persona
class Persona
  attr_accessor :nombre, :apellido, :edad

  def initialize(nombre, apellido, edad)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
  end

  def saludar
    "Hola, mi nombre es #{nombre} #{apellido} y tengo #{edad} años."
  end
end

# Clase Estudiante
class Estudiante < Persona
  attr_accessor :matricula, :promedio

  def initialize(nombre, apellido, edad, matricula, promedio)
    super(nombre, apellido, edad)
    @matricula = matricula
    @promedio = promedio
  end

  def estudiar
    "Estoy estudiando para mi examen final."
  end
end

# Clase Profesor
class Profesor < Persona
  attr_accessor :materia

  def initialize(nombre, apellido, edad, materia)
    super(nombre, apellido, edad)
    @materia = materia
  end

  def enseñar
    "Estoy enseñando #{materia} a mis estudiantes."
  end
end

# Creación de objetos

# Objeto de tipo Persona
persona1 = Persona.new("Juan", "García", 25)

# Objeto de tipo Estudiante
estudiante1 = Estudiante.new("María", "López", 20, "123456", 9.5)

# Objeto de tipo Profesor
profesor1 = Profesor.new("Pedro", "Sánchez", 40, "Matemáticas")

# Invocación de métodos

# Método saludar de la clase Persona
puts persona1.saludar

# Método estudiar de la clase Estudiante
puts estudiante1.estudiar

# Método enseñar de la clase Profesor
puts profesor1.enseñar
```

Explicación:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` es la clase base y tiene los atributos `nombre`, `apellido` y `edad`.
* La clase `Estudiante` hereda de la clase `Persona` y tiene los atributos adicionales `matricula` y `promedio`.
* La clase `Profesor` hereda de la clase `Persona` y tiene el atributo adicional `materia`.
* Se crean tres objetos: `persona1` de tipo `Persona`, `estudiante1` de tipo `Estudiante` y `profesor1` de tipo `Profesor`.
* Se invocan los métodos `saludar`, `estudiar` y `enseñar` de los objetos `persona1`, `estudiante1` y `profesor1`, respectivamente.