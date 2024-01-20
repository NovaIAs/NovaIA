```ruby
# Crea una clase llamada "Persona" con atributos "nombre" y "edad".
class Persona
  attr_accessor :nombre, :edad

  # Constructor de la clase "Persona".
  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  # Método de instancia para obtener el nombre de la persona.
  def nombre
    @nombre
  end

  # Método de instancia para obtener la edad de la persona.
  def edad
    @edad
  end

  # Método de instancia para saludar.
  def saludar
    puts "Hola, soy #{@nombre} y tengo #{@edad} años."
  end
end

# Crea dos objetos de tipo "Persona".
persona1 = Persona.new("Juan", 25)
persona2 = Persona.new("María", 30)

# Llama al método "saludar" para cada objeto.
persona1.saludar
persona2.saludar
```

Explicación:

* La clase "Persona" es una clase de Ruby que representa a una persona. Tiene dos atributos: "nombre" y "edad".
* El constructor de la clase "Persona" inicializa los atributos "nombre" y "edad" con los valores que se le pasan como argumentos.
* Los métodos de instancia "nombre" y "edad" devuelven los valores de los atributos correspondientes.
* El método de instancia "saludar" saluda al usuario.
* Se crean dos objetos de tipo "Persona" llamados "persona1" y "persona2".
* Se llama al método "saludar" para cada objeto.