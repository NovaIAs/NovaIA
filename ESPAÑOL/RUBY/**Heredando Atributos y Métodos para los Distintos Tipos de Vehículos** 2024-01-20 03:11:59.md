```ruby
# Definir una clase llamada "Vehiculo"
class Vehiculo
  # Atributos de la clase "Vehiculo"
  attr_accessor :marca, :modelo, :año, :color, :precio

  # Método constructor de la clase "Vehiculo"
  def initialize(marca, modelo, año, color, precio)
    @marca = marca
    @modelo = modelo
    @año = año
    @color = color
    @precio = precio
  end

  # Método para mostrar la información del vehículo
  def mostrar_informacion
    puts "Marca: #{@marca}"
    puts "Modelo: #{@modelo}"
    puts "Año: #{@año}"
    puts "Color: #{@color}"
    puts "Precio: #{@precio}"
  end
end

# Definir una clase llamada "Coche" que hereda de la clase "Vehiculo"
class Coche < Vehiculo
  # Atributos específicos de la clase "Coche"
  attr_accessor :numero_puertas, :tipo_combustible

  # Método constructor de la clase "Coche"
  def initialize(marca, modelo, año, color, precio, numero_puertas, tipo_combustible)
    super(marca, modelo, año, color, precio)
    @numero_puertas = numero_puertas
    @tipo_combustible = tipo_combustible
  end

  # Método para mostrar la información del coche
  def mostrar_informacion
    super
    puts "Número de puertas: #{@numero_puertas}"
    puts "Tipo de combustible: #{@tipo_combustible}"
  end
end

# Definir una clase llamada "Moto" que hereda de la clase "Vehiculo"
class Moto < Vehiculo
  # Atributos específicos de la clase "Moto"
  attr_accessor :cilindrada, :tipo_moto

  # Método constructor de la clase "Moto"
  def initialize(marca, modelo, año, color, precio, cilindrada, tipo_moto)
    super(marca, modelo, año, color, precio)
    @cilindrada = cilindrada
    @tipo_moto = tipo_moto
  end

  # Método para mostrar la información de la moto
  def mostrar_informacion
    super
    puts "Cilindrada: #{@cilindrada}"
    puts "Tipo de moto: #{@tipo_moto}"
  end
end

# Definir una clase llamada "Camión" que hereda de la clase "Vehiculo"
class Camión < Vehiculo
  # Atributos específicos de la clase "Camión"
  attr_accessor :carga_maxima, :tipo_camion

  # Método constructor de la clase "Camión"
  def initialize(marca, modelo, año, color, precio, carga_maxima, tipo_camion)
    super(marca, modelo, año, color, precio)
    @carga_maxima = carga_maxima
    @tipo_camion = tipo_camion
  end

  # Método para mostrar la información del camión
  def mostrar_informacion
    super
    puts "Carga máxima: #{@carga_maxima}"
    puts "Tipo de camión: #{@tipo_camion}"
  end
end

# Crear un objeto de tipo "Coche" llamado "mi_coche"
mi_coche = Coche.new("Toyota", "Corolla", 2023, "Rojo", 25000, 4, "Gasolina")

# Crear un objeto de tipo "Moto" llamado "mi_moto"
mi_moto = Moto.new("Honda", "CBR600RR", 2022, "Azul", 12000, 600, "Deportiva")

# Crear un objeto de tipo "Camión" llamado "mi_camion"
mi_camion = Camión.new("Mercedes-Benz", "Actros", 2021, "Blanco", 100000, 10000, "Pesado")

# Mostrar la información de los vehículos
puts "Información del coche:"
mi_coche.mostrar_informacion

puts "Información de la moto:"
mi_moto.mostrar_informacion

puts "Información del camión:"
mi_camion.mostrar_informacion
```

Explicación del código:

* **Clases:** Se definieron cuatro clases: "Vehiculo", "Coche", "Moto" y "Camión". La clase "Vehiculo" es la clase base de todas las demás clases. Las clases "Coche", "Moto" y "Camión" heredan de la clase "Vehiculo" y añaden atributos y métodos específicos para cada tipo de vehículo.

* **Atributos:** Cada clase tiene sus propios atributos. La clase "Vehiculo" tiene los atributos `marca`, `modelo`, `año`, `color` y `precio`. La clase "Coche" tiene atributos adicionales como `numero_puertas` y `tipo_combustible`. La clase "Moto" tiene atributos adicionales como `cilindrada` y `tipo_moto`. La clase "Camión" tiene atributos adicionales como `carga_maxima` y `tipo_camion`.

* **Métodos:** Cada clase tiene sus propios métodos. La clase "Vehiculo" tiene un método llamado `mostrar_informacion` que muestra la información del vehículo. La clase "Coche", "Moto" y "Camión" sobrescriben el método `mostrar_informacion` para mostrar la información específica de cada tipo de vehículo.

* **Objetos:** Se crearon tres objetos: `mi_coche`, `mi_moto` y `mi_camion`. Cada objeto es una instancia de una clase específica. El objeto `mi_coche` es una instancia de la clase "Coche", el objeto `mi_moto` es una instancia de la clase "Moto" y el objeto `mi_camion` es una instancia de la clase "Camión".

* **Mostrar la información:** Por último, se utilizaron los métodos `mostrar_informacion` de los objetos para mostrar la información de los vehículos en la consola.