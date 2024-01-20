```
# Recopilar información de un usuario
puts "Bienvenido a nuestro sistema de información."
print "Por favor, ingresa tu nombre: "
nombre = gets.chomp

print "¿Cuál es tu edad?"
edad = gets.chomp

print "¿Cuál es tu ocupación?"
ocupacion = gets.chomp

# Crear una clase para representar a un usuario
class Usuario
  attr_accessor :nombre, :edad, :ocupacion

  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  def saludar
    "Hola, mi nombre es #{@nombre} y tengo #{@edad} años. Soy #{@ocupacion}."
  end
end

# Crear un objeto de usuario
usuario = Usuario.new(nombre, edad, ocupacion)

# Imprimir los datos del usuario
puts "Nombre: #{usuario.nombre}"
puts "Edad: #{usuario.edad}"
puts "Ocupación: #{usuario.ocupacion}"

# Llamar al método saludar del usuario
puts usuario.saludar
```

Este código es más complejo que el anterior porque utiliza una clase y un objeto para representar a un usuario. La clase tiene atributos para almacenar el nombre, la edad y la ocupación del usuario, y métodos para acceder y modificar estos atributos. El objeto se crea a partir de la clase y se utilizan los métodos para acceder y modificar los atributos del objeto.

El código también utiliza la instrucción `puts` para imprimir los datos del usuario en la consola. La instrucción `puts` toma una expresión como argumento y la imprime en la consola. En este caso, la expresión es el objeto `usuario`, que se imprime como una cadena que contiene los datos del usuario.

Este código es más complejo que el anterior, pero también es más flexible y reutilizable. La clase `Usuario` se puede utilizar para representar a cualquier usuario, y el objeto `usuario` se puede utilizar para almacenar y acceder a los datos de un usuario específico.