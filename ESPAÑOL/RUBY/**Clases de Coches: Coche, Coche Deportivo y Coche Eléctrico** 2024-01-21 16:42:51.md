```ruby
# Definición de la clase Coche
class Coche
  # Atributos de la clase Coche
  attr_accessor :marca, :modelo, :color, :año

  # Constructor de la clase Coche
  def initialize(marca, modelo, color, año)
    @marca = marca
    @modelo = modelo
    @color = color
    @año = año
  end

  # Método para mostrar la información del coche
  def mostrar_informacion
    puts "Marca: #{@marca}"
    puts "Modelo: #{@modelo}"
    puts "Color: #{@color}"
    puts "Año: #{@año}"
  end
end

# Definición de la clase Coche Deportivo, que hereda de la clase Coche
class CocheDeportivo < Coche
  # Atributos adicionales de la clase Coche Deportivo
  attr_accessor :velocidad_maxima

  # Constructor de la clase Coche Deportivo
  def initialize(marca, modelo, color, año, velocidad_maxima)
    super(marca, modelo, color, año)
    @velocidad_maxima = velocidad_maxima
  end

  # Método para mostrar la información del coche deportivo
  def mostrar_informacion
    super
    puts "Velocidad Máxima: #{@velocidad_maxima}"
  end
end

# Definición de la clase Coche Eléctrico, que hereda de la clase Coche
class CocheEléctrico < Coche
  # Atributos adicionales de la clase Coche Eléctrico
  attr_accessor :autonomía

  # Constructor de la clase Coche Eléctrico
  def initialize(marca, modelo, color, año, autonomía)
    super(marca, modelo, color, año)
    @autonomía = autonomía
  end

  # Método para mostrar la información del coche eléctrico
  def mostrar_informacion
    super
    puts "Autonomía: #{@autonomía}"
  end
end

# Creación de un objeto de la clase Coche
coche = Coche.new("Toyota", "Corolla", "Rojo", 2020)

# Mostrar la información del coche
coche.mostrar_informacion

# Creación de un objeto de la clase Coche Deportivo
coche_deportivo = CocheDeportivo.new("Ferrari", "458 Italia", "Rojo", 2015, 330)

# Mostrar la información del coche deportivo
coche_deportivo.mostrar_informacion

# Creación de un objeto de la clase Coche Eléctrico
coche_eléctrico = CocheEléctrico.new("Tesla", "Model S", "Blanco", 2022, 628)

# Mostrar la información del coche eléctrico
coche_eléctrico.mostrar_informacion
```

Este código define tres clases de coches: Coche, Coche Deportivo y Coche Eléctrico. La clase Coche define los atributos comunes a todos los coches, como la marca, el modelo, el color y el año. La clase Coche Deportivo hereda de la clase Coche y añade el atributo velocidad_maxima. La clase Coche Eléctrico también hereda de la clase Coche y añade el atributo autonomía.

El código crea tres objetos: un objeto de la clase Coche, un objeto de la clase Coche Deportivo y un objeto de la clase Coche Eléctrico. A continuación, llama al método mostrar_informacion() en cada objeto para mostrar su información.

Este código es complejo porque utiliza la herencia, la polimorfía y la encapsulación. Es un ejemplo de cómo se pueden construir clases complejas a partir de clases más simples.