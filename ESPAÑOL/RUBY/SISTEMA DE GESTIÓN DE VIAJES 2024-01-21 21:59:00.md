```ruby
# Crear una clase llamada "Conductor" con los siguientes atributos:

class Conductor
  attr_accessor :nombre, :edad, :licencia

  def initialize(nombre, edad, licencia)
    @nombre = nombre
    @edad = edad
    @licencia = licencia
  end

  # Método para imprimir la información del conductor.
  def imprimir_informacion
    puts "Nombre: #{@nombre}"
    puts "Edad: #{@edad}"
    puts "Licencia: #{@licencia}"
  end
end

# Crear una clase llamada "Vehiculo" con los siguientes atributos:

class Vehiculo
  attr_accessor :marca, :modelo, :año, :color, :placas

  def initialize(marca, modelo, año, color, placas)
    @marca = marca
    @modelo = modelo
    @año = año
    @color = color
    @placas = placas
  end

  # Método para imprimir la información del vehículo.
  def imprimir_informacion
    puts "Marca: #{@marca}"
    puts "Modelo: #{@modelo}"
    puts "Año: #{@año}"
    puts "Color: #{@color}"
    puts "Placas: #{@placas}"
  end
end

# Crear una clase llamada "Viaje" con los siguientes atributos:

class Viaje
  attr_accessor :origen, :destino, :fecha, :hora, :distancia

  def initialize(origen, destino, fecha, hora, distancia)
    @origen = origen
    @destino = destino
    @fecha = fecha
    @hora = hora
    @distancia = distancia
  end

  # Método para imprimir la información del viaje.
  def imprimir_informacion
    puts "Origen: #{@origen}"
    puts "Destino: #{@destino}"
    puts "Fecha: #{@fecha}"
    puts "Hora: #{@hora}"
    puts "Distancia: #{@distancia}"
  end
end

# Crear un objeto de la clase "Conductor" con los siguientes valores:

conductor = Conductor.new("Juan Pérez", 35, "A-123456")

# Crear un objeto de la clase "Vehiculo" con los siguientes valores:

vehiculo = Vehiculo.new("Toyota", "Corolla", 2020, "Rojo", "ABC-123")

# Crear un objeto de la clase "Viaje" con los siguientes valores:

viaje = Viaje.new("Madrid", "Barcelona", "2023-03-08", "10:00", 600)

# Imprimir la información del conductor, el vehículo y el viaje.

puts "Información del conductor:"
conductor.imprimir_informacion

puts "\nInformación del vehículo:"
vehiculo.imprimir_informacion

puts "\nInformación del viaje:"
viaje.imprimir_informacion

# Crear un nuevo objeto de la clase "Conductor" con diferentes valores.

conductor2 = Conductor.new("María García", 40, "B-234567")

# Asignar el nuevo objeto "conductor2" al atributo "conductor" del objeto "vehiculo".

vehiculo.conductor = conductor2

# Imprimir la información del conductor y el vehículo.

puts "\nInformación del conductor del vehículo:"
vehiculo.conductor.imprimir_informacion

puts "\nInformación del vehículo:"
vehiculo.imprimir_informacion
```

Explicación del código:

1. Se crean tres clases: "Conductor", "Vehiculo" y "Viaje", cada una con sus propios atributos.
2. Se crean objetos de cada clase con valores específicos.
3. Se imprimen los datos de los objetos de cada clase.
4. Se crea un nuevo objeto de la clase "Conductor" y se asigna al atributo "conductor" del objeto "vehiculo".
5. Se imprimen nuevamente los datos del conductor y del vehículo, mostrando cómo el nuevo objeto "conductor2" ha reemplazado al anterior.

Este código demuestra el uso de clases, objetos y atributos en Ruby, así como la interacción entre objetos de diferentes clases.