```ruby
# Este código define una clase llamada "Casa" que representa una casa con sus características.

class Casa
  # Definimos los atributos de la casa.
  attr_accessor :direccion, :habitaciones, :baños, :metros_cuadrados

  # Definimos el método initialize para inicializar una nueva casa.
  def initialize(direccion, habitaciones, baños, metros_cuadrados)
    @direccion = direccion
    @habitaciones = habitaciones
    @baños = baños
    @metros_cuadrados = metros_cuadrados
  end

  # Definimos un método llamado "to_s" para mostrar la información de la casa.
  def to_s
    "Casa en #{direccion} con #{habitaciones} habitaciones, #{baños} baños y #{metros_cuadrados} metros cuadrados."
  end
end

# Creamos una nueva instancia de la clase "Casa".
casa1 = Casa.new("Calle Mayor 123", 3, 2, 150)

# Mostramos la información de la casa.
puts casa1

# Creamos una nueva instancia de la clase "Casa" y la asignamos a una variable.
casa2 = Casa.new("Avenida del Sol 456", 4, 3, 200)

# Comparando dos objetos "Casa" en términos de sus direcciones.
if casa1.direccion == casa2.direccion
  puts "Las casas tienen la misma dirección."
else
  puts "Las casas tienen diferentes direcciones."
end

# Creamos un array de casas.
casas = [casa1, casa2]

# Iteramos sobre el array de casas y mostramos la información de cada casa.
casas.each do |casa|
  puts casa
end
```

Explicación del código:

1. Definimos una clase llamada "Casa" que representa una casa con sus características.
2. Definimos los atributos de la casa (dirección, habitaciones, baños y metros cuadrados).
3. Definimos el método "initialize" para inicializar una nueva casa.
4. Definimos un método llamado "to_s" para mostrar la información de la casa.
5. Creamos una nueva instancia de la clase "Casa" y la asignamos a una variable.
6. Mostramos la información de la casa.
7. Creamos una nueva instancia de la clase "Casa" y la asignamos a otra variable.
8. Comparando dos objetos "Casa" en términos de sus direcciones.
9. Creamos un array de casas.
10. Iteramos sobre el array de casas y mostramos la información de cada casa.