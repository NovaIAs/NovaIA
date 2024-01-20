```ruby
# Definir una función para generar un saludo personalizado

def saludar(nombre)
  "¡Hola, #{nombre}! ¿Cómo estás hoy?"
end

# Crear una clase Persona con los atributos nombre y edad

class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  def presentarse
    "Hola, mi nombre es #{nombre} y tengo #{edad} años."
  end
end

# Crear una lista de nombres de personas

nombres = ["Juan", "María", "Pedro", "Ana"]

# Crear una lista de objetos Persona

personas = []
nombres.each do |nombre|
  persona = Persona.new(nombre, rand(18..65))
  personas << persona
end

# Iterar sobre la lista de objetos Persona y saludar a cada uno

personas.each do |persona|
  puts saludar(persona.nombre)
end

# Buscar a la persona más joven y a la más vieja

persona_mas_joven = personas.min_by { |persona| persona.edad }
persona_mas_vieja = personas.max_by { |persona| persona.edad }

# Imprimir el nombre y la edad de la persona más joven y de la más vieja

puts "La persona más joven es #{persona_mas_joven.nombre} con #{persona_mas_joven.edad} años."
puts "La persona más vieja es #{persona_mas_vieja.nombre} con #{persona_mas_vieja.edad} años."
```

Explicación del código:

1. Se define una función `saludar` que toma un nombre como parámetro y devuelve un mensaje personalizado de saludo.
2. Se crea una clase `Persona` con los atributos `nombre` y `edad`.
3. Se crea una lista de nombres de personas.
4. Se crea una lista de objetos `Persona` a partir de la lista de nombres.
5. Se itera sobre la lista de objetos `Persona` y se saluda a cada uno.
6. Se busca a la persona más joven y a la más vieja de la lista.
7. Se imprime el nombre y la edad de la persona más joven y de la más vieja.