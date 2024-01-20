```ruby
# Clase Persona
class Persona
  attr_accessor :nombre, :edad, :estatura

  def initialize(nombre, edad, estatura)
    @nombre = nombre
    @edad = edad
    @estatura = estatura
  end

  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Estatura: #{@estatura}"
  end
end

# Clase Estudiante
class Estudiante < Persona
  attr_accessor :matricula, :promedio

  def initialize(nombre, edad, estatura, matricula, promedio)
    super(nombre, edad, estatura)
    @matricula = matricula
    @promedio = promedio
  end

  def to_s
    "#{super}, Matricula: #{@matricula}, Promedio: #{@promedio}"
  end
end

# Clase Profesor
class Profesor < Persona
  attr_accessor :titulo, :salario

  def initialize(nombre, edad, estatura, titulo, salario)
    super(nombre, edad, estatura)
    @titulo = titulo
    @salario = salario
  end

  def to_s
    "#{super}, Título: #{@titulo}, Salario: #{@salario}"
  end
end

# Crear objetos
estudiante1 = Estudiante.new("Juan", 20, 1.75, "A12345", 8.5)
estudiante2 = Estudiante.new("María", 21, 1.65, "B12345", 9.0)

profesor1 = Profesor.new("Carlos", 40, 1.80, "Licenciado", 3000)
profesor2 = Profesor.new("Ana", 35, 1.70, "Doctora", 4000)

# Almacenar objetos en un array
personas = [estudiante1, estudiante2, profesor1, profesor2]

# Recorrer el array e imprimir los datos de cada objeto
personas.each do |persona|
  puts persona
end
```

Explicación del código:

1. Clase `Persona`: Es una clase base que define los atributos y métodos comunes a todas las personas. Tiene los atributos `nombre`, `edad` y `estatura`.

2. Clase `Estudiante`: Es una clase que hereda de la clase `Persona` y añade los atributos `matricula` y `promedio`.

3. Clase `Profesor`: Es una clase que hereda de la clase `Persona` y añade los atributos `titulo` y `salario`.

4. Se crean objetos de las clases `Estudiante` y `Profesor`.

5. Se almacenan los objetos en un array llamado `personas`.

6. Se recorre el array `personas` y se imprime la información de cada objeto.

Este código es complejo porque:

* Utiliza herencia de clases para crear jerarquías de objetos.
* Define múltiples clases con diferentes atributos y métodos.
* Implementa la sobrecarga de métodos para permitir que los objetos de diferentes clases respondan al mismo método de diferentes maneras.
* Utiliza un array para almacenar objetos de diferentes clases.
* Recorre el array utilizando el método `each` para imprimir la información de cada objeto.

Este código es un ejemplo de cómo se puede utilizar la programación orientada a objetos en Ruby para crear programas complejos y bien organizados.