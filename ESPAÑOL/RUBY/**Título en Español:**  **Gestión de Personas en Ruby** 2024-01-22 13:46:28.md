```ruby
# Crea una clase "Persona" con atributos "nombre" y "edad"
class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}"
  end
end

# Crea un objeto "persona1" de tipo "Persona"
persona1 = Persona.new("Juan", 25)

# Imprime los atributos de "persona1"
puts "Nombre: #{persona1.nombre}, Edad: #{persona1.edad}"

# Modifica el atributo "edad" de "persona1"
persona1.edad = 30

# Imprime nuevamente los atributos de "persona1"
puts "Nombre: #{persona1.nombre}, Edad: #{persona1.edad}"

# Crea un array de objetos "Persona"
personas = [
  Persona.new("María", 28),
  Persona.new("Pedro", 32),
  Persona.new("Ana", 22)
]

# Imprime el array de objetos "Persona"
puts personas

# Filtra el array "personas" para obtener solo las personas mayores de 25 años
personas_mayores_de_25 = personas.select { |persona| persona.edad > 25 }

# Imprime el array filtrado
puts personas_mayores_de_25

# Ordena el array "personas" por edad en orden descendente
personas.sort_by! { |persona| -persona.edad }

# Imprime el array ordenado
puts personas

# Agrupa el array "personas" por edad
personas_agrupadas_por_edad = personas.group_by { |persona| persona.edad }

# Imprime el hash de objetos "Persona" agrupados por edad
puts personas_agrupadas_por_edad
```

**Explicación del código:**

1. Primero, se crea una clase `Persona` con dos atributos: `nombre` y `edad`. La clase tiene un constructor que inicializa estos atributos y un método `to_s` que devuelve una representación en cadena de la persona.


2. A continuación, se crea un objeto `persona1` de tipo `Persona` y se imprimen sus atributos.


3. Luego, se modifica el atributo `edad` de `persona1` y se imprime de nuevo.


4. Seguidamente, se crea un array de objetos `Persona` y se imprime el array.


5. Después, se filtra el array `personas` para obtener solo las personas mayores de 25 años y se imprime el array filtrado.


6. A continuación, se ordena el array `personas` por edad en orden descendente y se imprime el array ordenado.


7. Finalmente, se agrupa el array `personas` por edad y se imprime el hash de objetos `Persona` agrupados por edad.