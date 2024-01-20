```ruby
# Definir una clase llamada "Coche" con atributos y métodos

class Coche
  attr_accessor :marca, :modelo, :año, :color, :precio

  def initialize(marca, modelo, año, color, precio)
    @marca = marca
    @modelo = modelo
    @año = año
    @color = color
    @precio = precio
  end

  def encender
    puts "El coche de marca #{@marca} y modelo #{@modelo} ha sido encendido."
  end

  def apagar
    puts "El coche de marca #{@marca} y modelo #{@modelo} ha sido apagado."
  end

  def acelerar
    puts "El coche de marca #{@marca} y modelo #{@modelo} está acelerando."
  end

  def frenar
    puts "El coche de marca #{@marca} y modelo #{@modelo} está frenando."
  end
end

# Crear objetos de la clase "Coche"

coche1 = Coche.new("Toyota", "Corolla", 2023, "Rojo", 25000)
coche2 = Coche.new("Honda", "Civic", 2022, "Azul", 22000)

# Mostrar información de los coches

puts "Información del coche 1:"
puts "Marca: #{coche1.marca}"
puts "Modelo: #{coche1.modelo}"
puts "Año: #{coche1.año}"
puts "Color: #{coche1.color}"
puts "Precio: #{coche1.precio}"

puts "\nInformación del coche 2:"
puts "Marca: #{coche2.marca}"
puts "Modelo: #{coche2.modelo}"
puts "Año: #{coche2.año}"
puts "Color: #{coche2.color}"
puts "Precio: #{coche2.precio}"

# Utilizar métodos de los coches

coche1.encender
coche2.apagar
coche1.acelerar
coche2.frenar
```

Explicación del código:

* La clase `Coche` se define con los atributos `marca`, `modelo`, `año`, `color` y `precio`.
* Se define un constructor para la clase `Coche` que recibe como argumentos los valores de los atributos y los asigna a las variables de instancia correspondientes.
* Se definen los métodos `encender`, `apagar`, `acelerar` y `frenar` para la clase `Coche`. Estos métodos simplemente imprimen un mensaje en la consola indicando la acción que se está realizando.
* Se crean dos objetos de la clase `Coche`, `coche1` y `coche2`, asignándoles valores a sus atributos.
* Se muestran los valores de los atributos de los coches `coche1` y `coche2` en la consola.
* Se utilizan los métodos `encender`, `apagar`, `acelerar` y `frenar` de los coches `coche1` y `coche2` para realizar las acciones correspondientes.