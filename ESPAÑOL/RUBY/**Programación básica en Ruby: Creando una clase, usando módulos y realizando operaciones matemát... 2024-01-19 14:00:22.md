```ruby
# Definición de una clase llamada "Persona" con atributos "nombre" y "edad".
class Persona
  attr_accessor :nombre, :edad

  # Método de inicialización con parámetros "nombre" y "edad".
  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  # Método para obtener el nombre de la persona.
  def obtener_nombre
    @nombre
  end

  # Método para obtener la edad de la persona.
  def obtener_edad
    @edad
  end

  # Método para saludar con el nombre de la persona.
  def saludar
    "Hola, mi nombre es #{obtener_nombre} y tengo #{obtener_edad} años."
  end
end

# Creación de dos objetos de la clase "Persona".
persona1 = Persona.new("Juan", 25)
persona2 = Persona.new("María", 30)

# Impresión del saludo de cada persona.
puts persona1.saludar
puts persona2.saludar

# Definición de un módulo llamado "Calculadora" con métodos matemáticos básicos.
module Calculadora
  # Método para sumar dos números.
  def sumar(a, b)
    a + b
  end

  # Método para restar dos números.
  def restar(a, b)
    a - b
  end

  # Método para multiplicar dos números.
  def multiplicar(a, b)
    a * b
  end

  # Método para dividir dos números.
  def dividir(a, b)
    a / b
  end
end

# Inclusión del módulo "Calculadora" en la clase "Persona".
class Persona
  include Calculadora
end

# Uso de los métodos matemáticos básicos de la clase "Persona".
resultado1 = persona1.sumar(10, 15)
resultado2 = persona2.restar(20, 10)
resultado3 = persona1.multiplicar(5, 3)
resultado4 = persona2.dividir(30, 5)

# Impresión de los resultados de las operaciones matemáticas.
puts "El resultado de la suma es: #{resultado1}"
puts "El resultado de la resta es: #{resultado2}"
puts "El resultado de la multiplicación es: #{resultado3}"
puts "El resultado de la división es: #{resultado4}"
```

Explicación del código:

* Se define una clase llamada "Persona" con los atributos "nombre" y "edad".
* Se crea un método de inicialización para la clase "Persona" que recibe como parámetros "nombre" y "edad" y los asigna a los atributos correspondientes.
* Se definen métodos para obtener el nombre y la edad de la persona, y un método para saludar con el nombre de la persona.
* Se crean dos objetos de la clase "Persona" llamados "persona1" y "persona2" con diferentes valores de nombre y edad.
* Se imprime el saludo de cada persona utilizando el método "saludar".
* Se define un módulo llamado "Calculadora" con métodos matemáticos básicos.
* Se incluye el módulo "Calculadora" en la clase "Persona".
* Se utilizan los métodos matemáticos básicos de la clase "Persona" para realizar operaciones matemáticas con los objetos "persona1" y "persona2".
* Se imprimen los resultados de las operaciones matemáticas.