```ruby
# Definir una clase Persona con atributos nombre, edad y sexo
class Persona
  attr_accessor :nombre, :edad, :sexo

  # Constructor de la clase Persona
  def initialize(nombre, edad, sexo)
    @nombre = nombre
    @edad = edad
    @sexo = sexo
  end

  # Método para saludar
  def saludar
    puts "Hola, mi nombre es #{@nombre}."
  end
end

# Definir una clase Estudiante que hereda de la clase Persona y añade el atributo calificacion
class Estudiante < Persona
  attr_accessor :calificacion

  # Constructor de la clase Estudiante
  def initialize(nombre, edad, sexo, calificacion)
    super(nombre, edad, sexo)
    @calificacion = calificacion
  end

  # Método para estudiar
  def estudiar
    puts "Estoy estudiando."
  end
end

# Definir una clase Profesor que hereda de la clase Persona y añade el atributo materia
class Profesor < Persona
  attr_accessor :materia

  # Constructor de la clase Profesor
  def initialize(nombre, edad, sexo, materia)
    super(nombre, edad, sexo)
    @materia = materia
  end

  # Método para enseñar
  def ensenar
    puts "Estoy enseñando #{materia}."
  end
end

# Crear una instancia de la clase Persona
persona = Persona.new("Juan", 20, "Masculino")

# Crear una instancia de la clase Estudiante
estudiante = Estudiante.new("María", 18, "Femenino", 9.5)

# Crear una instancia de la clase Profesor
profesor = Profesor.new("Pedro", 35, "Masculino", "Matemáticas")

# Llamar al método saludar de la instancia de la clase Persona
persona.saludar

# Llamar al método estudiar de la instancia de la clase Estudiante
estudiante.estudiar

# Llamar al método ensenar de la instancia de la clase Profesor
profesor.ensenar
```

Este código es un ejemplo de código complejo en Ruby que utiliza herencia y polimorfismo.

La clase `Persona` es una clase base que define los atributos y métodos comunes a todas las personas. La clase `Estudiante` hereda de la clase `Persona` y añade el atributo `calificacion` y el método `estudiar`. La clase `Profesor` también hereda de la clase `Persona` y añade el atributo `materia` y el método `ensenar`.

El código crea una instancia de cada una de las clases y llama a los métodos correspondientes para demostrar su funcionalidad.

Este código es complejo porque utiliza herencia y polimorfismo, que son conceptos avanzados en programación orientada a objetos. Sin embargo, el código está bien documentado y es fácil de entender.