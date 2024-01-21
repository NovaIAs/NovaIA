```ruby
# FUNCIONES

# Función para saludar
def saludar(nombre)
  puts "Hola, #{nombre}!"
end

# Función para sumar dos números
def sumar(a, b)
  a + b
end

# Función para restar dos números
def restar(a, b)
  a - b
end

# Función para multiplicar dos números
def multiplicar(a, b)
  a * b
end

# Función para dividir dos números
def dividir(a, b)
  a / b
end

# CLASES

# Clase Persona
class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  def presentarse
    puts "Hola, mi nombre es #{nombre} y tengo #{edad} años."
  end
end

# Clase Empleado
class Empleado < Persona
  attr_accessor :salario

  def initialize(nombre, edad, salario)
    super(nombre, edad)
    @salario = salario
  end

  def presentarse
    super
    puts "Soy empleado y mi salario es #{salario}."
  end
end

# Clase Estudiante
class Estudiante < Persona
  attr_accessor :universidad

  def initialize(nombre, edad, universidad)
    super(nombre, edad)
    @universidad = universidad
  end

  def presentarse
    super
    puts "Soy estudiante y estudio en la #{universidad}."
  end
end

# MAIN

# Crear un objeto de la clase Persona
persona = Persona.new("Juan", 25)

# Crear un objeto de la clase Empleado
empleado = Empleado.new("María", 30, 1000)

# Crear un objeto de la clase Estudiante
estudiante = Estudiante.new("Pedro", 20, "Universidad de Sevilla")

# Llamar al método presentarse para cada objeto
persona.presentarse
empleado.presentarse
estudiante.presentarse

# Utilizar las funciones definidas anteriormente
puts "La suma de 1 y 2 es #{sumar(1, 2)}"
puts "La resta de 3 y 2 es #{restar(3, 2)}"
puts "La multiplicación de 4 y 5 es #{multiplicar(4, 5)}"
puts "La división de 10 y 2 es #{dividir(10, 2)}"
```

Este código es un ejemplo de cómo se puede usar la programación orientada a objetos en Ruby. Se definen varias funciones y clases, y luego se crean objetos de esas clases y se llaman a sus métodos. El código también muestra cómo se pueden usar las funciones definidas anteriormente para realizar cálculos.