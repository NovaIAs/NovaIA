```ruby
# Módulo para definir una clase Persona
module Persona
  # Clase Persona con atributos nombre y edad
  class Persona
    attr_accessor :nombre, :edad

    # Constructor de la clase Persona
    def initialize(nombre, edad)
      @nombre = nombre
      @edad = edad
    end

    # Método to_s para imprimir la información de una persona
    def to_s
      "Nombre: #{@nombre}, Edad: #{@edad}"
    end
  end
end

# Módulo para definir una clase Estudiante
module Estudiante
  # Clase Estudiante con atributos nombre, edad y nota
  class Estudiante < Persona::Persona
    attr_accessor :nota

    # Constructor de la clase Estudiante
    def initialize(nombre, edad, nota)
      super(nombre, edad)
      @nota = nota
    end

    # Método to_s para imprimir la información de un estudiante
    def to_s
      "#{super}, Nota: #{@nota}"
    end
  end
end

# Módulo para definir una clase Profesor
module Profesor
  # Clase Profesor con atributos nombre, edad y asignatura
  class Profesor < Persona::Persona
    attr_accessor :asignatura

    # Constructor de la clase Profesor
    def initialize(nombre, edad, asignatura)
      super(nombre, edad)
      @asignatura = asignatura
    end

    # Método to_s para imprimir la información de un profesor
    def to_s
      "#{super}, Asignatura: #{@asignatura}"
    end
  end
end

# Creación de objetos Persona, Estudiante y Profesor
persona1 = Persona::Persona.new("Juan", 20)
estudiante1 = Estudiante::Estudiante.new("María", 18, 9)
profesor1 = Profesor::Profesor.new("Luis", 35, "Matemáticas")

# Impresión de la información de los objetos
puts "Persona 1: #{persona1}"
puts "Estudiante 1: #{estudiante1}"
puts "Profesor 1: #{profesor1}"
```

Explicación del código:

1. Se definen tres módulos: `Persona`, `Estudiante` y `Profesor`. Cada módulo contiene una clase con el mismo nombre que el módulo.

2. La clase `Persona` tiene dos atributos: `nombre` y `edad`. La clase `Estudiante` hereda de la clase `Persona` y añade un nuevo atributo: `nota`. La clase `Profesor` también hereda de la clase `Persona` y añade un nuevo atributo: `asignatura`.

3. Cada clase tiene un constructor que inicializa los atributos de la clase.

4. Cada clase tiene un método `to_s` que devuelve una cadena con la información de la clase.

5. Se crean tres objetos: `persona1` de tipo `Persona`, `estudiante1` de tipo `Estudiante` y `profesor1` de tipo `Profesor`.

6. Se imprime la información de los objetos.