```ruby
# Crear una clase llamada "Persona" que tenga los siguientes atributos:
# - nombre
# - apellido
# - edad
class Persona
attr_accessor :nombre, :apellido, :edad

# Crear el método "hablar" que imprima un saludo con el nombre de la persona
def hablar
puts "Hola, mi nombre es #{nombre}"
end
end

# Crear una clase llamada "Estudiante" que herede de la clase "Persona" y tenga los siguientes atributos adicionales:
# - matricula
# - carrera
class Estudiante < Persona
attr_accessor :matricula, :carrera

# Crear el método "estudiar" que imprima un mensaje de que el estudiante está estudiando
def estudiar
puts "#{nombre} está estudiando #{carrera}"
end
end

# Crear una clase llamada "Profesor" que herede de la clase "Persona" y tenga los siguientes atributos adicionales:
# - asignatura
# - años_de_experiencia
class Profesor < Persona
attr_accessor :asignatura, :años_de_experiencia

# Crear el método "enseñar" que imprima un mensaje de que el profesor está enseñando
def enseñar
puts "#{nombre} está enseñando #{asignatura}"
end
end

# Crear un objeto de la clase "Persona" con los siguientes valores:
# - nombre: "Juan"
# - apellido: "Pérez"
# - edad: 25
persona = Persona.new
persona.nombre = "Juan"
persona.apellido = "Pérez"
persona.edad = 25

# Imprimir el saludo de la persona
persona.hablar

# Crear un objeto de la clase "Estudiante" con los siguientes valores:
# - nombre: "María"
# - apellido: "García"
# - edad: 20
# - matricula: "123456"
# - carrera: "Ingeniería en Sistemas"
estudiante = Estudiante.new
estudiante.nombre = "María"
estudiante.apellido = "García"
estudiante.edad = 20
estudiante.matricula = "123456"
estudiante.carrera = "Ingeniería en Sistemas"

# Imprimir un mensaje de que el estudiante está estudiando
estudiante.estudiar

# Crear un objeto de la clase "Profesor" con los siguientes valores:
# - nombre: "Pedro"
# - apellido: "López"
# - edad: 40
# - asignatura: "Matemáticas"
# - años_de_experiencia: 10
profesor = Profesor.new
profesor.nombre = "Pedro"
profesor.apellido = "López"
profesor.edad = 40
profesor.asignatura = "Matemáticas"
profesor.años_de_experiencia = 10

# Imprimir un mensaje de que el profesor está enseñando
profesor.enseñar
```

Este código es más complejo que el anterior porque define tres clases diferentes: "Persona", "Estudiante" y "Profesor". Cada clase tiene sus propios atributos y métodos, y la clase "Estudiante" y "Profesor" heredan de la clase "Persona".

El código también crea tres objetos de diferentes clases y llama a sus métodos para imprimir mensajes.

Este código es un ejemplo de cómo se puede usar la herencia en Ruby para crear clases que comparten características comunes.