```ruby
# Definición de la clase Persona
class Persona
  attr_accessor :nombre, :apellido, :edad

  def initialize(nombre, apellido, edad)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
  end

  def saludar
    "Hola, mi nombre es #{@nombre} #{@apellido} y tengo #{@edad} años."
  end
end

# Definición de la clase Estudiante
class Estudiante < Persona
  attr_accessor :matricula, :promedio

  def initialize(nombre, apellido, edad, matricula, promedio)
    super(nombre, apellido, edad)
    @matricula = matricula
    @promedio = promedio
  end

  def estudiar
    "Estoy estudiando para mi examen de mañana."
  end
end

# Definición de la clase Profesor
class Profesor < Persona
  attr_accessor :asignatura, :salario

  def initialize(nombre, apellido, edad, asignatura, salario)
    super(nombre, apellido, edad)
    @asignatura = asignatura
    @salario = salario
  end

  def dar_clase
    "Estoy dando clase de #{@asignatura}."
  end
end

# Creación de objetos
persona1 = Persona.new("Juan", "García", 25)
estudiante1 = Estudiante.new("María", "López", 18, "123456", 9.5)
profesor1 = Profesor.new("Pedro", "Sánchez", 45, "Matemáticas", 2500)

# Impresión de los resultados
puts persona1.saludar
puts estudiante1.saludar
puts estudiante1.estudiar
puts profesor1.saludar
puts profesor1.dar_clase
```

Explicación del código:

* Se definen tres clases: `Persona`, `Estudiante` y `Profesor`.
* La clase `Persona` es la clase base, y las clases `Estudiante` y `Profesor` heredan de ella.
* Las clases `Estudiante` y `Profesor` tienen atributos adicionales que no tiene la clase `Persona`.
* Se crean tres objetos: un objeto de la clase `Persona`, un objeto de la clase `Estudiante` y un objeto de la clase `Profesor`.
* Se imprimen los resultados de llamar a los métodos `saludar`, `estudiar` y `dar_clase` de los tres objetos.

Este código es complejo porque:

* Utiliza herencia de clases, que es un concepto avanzado de programación orientada a objetos.
* Tiene múltiples clases con diferentes atributos y métodos.
* Crea múltiples objetos de diferentes clases.
* Utiliza métodos de diferentes clases para imprimir los resultados.