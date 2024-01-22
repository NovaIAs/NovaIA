```ruby
# Definir un hash con información de personas.
personas = {
  "Juan" => {
    "edad" => 25,
    "ocupación" => "Desarrollador de software",
    "hobbies" => ["Correr", "Leer", "Viajar"]
  },
  "María" => {
    "edad" => 30,
    "ocupación" => "Médico",
    "hobbies" => ["Cocinar", "Bailar", "Cantar"]
  },
  "Pedro" => {
    "edad" => 40,
    "ocupación" => "Abogado",
    "hobbies" => ["Jugar fútbol", "Ver películas", "Tocar la guitarra"]
  }
}

# Crear un método para imprimir la información de una persona.
def imprimir_persona(persona)
  puts "Nombre: #{persona["nombre"]}"
  puts "Edad: #{persona["edad"]}"
  puts "Ocupación: #{persona["ocupación"]}"
  puts "Hobbies:"
  persona["hobbies"].each do |hobby|
    puts "- #{hobby}"
  end
end

# Recorrer el hash de personas e imprimir la información de cada una.
personas.each do |nombre, persona|
  imprimir_persona(persona)
  puts "\n"
end
```

Explicación:

* El hash `personas` contiene información sobre tres personas, incluyendo su nombre, edad, ocupación y hobbies.
* El método `imprimir_persona` toma una persona como argumento e imprime su información en la consola.
* El bucle `personas.each` recorre el hash `personas` e imprime la información de cada persona llamando al método `imprimir_persona`.

Este código es complejo porque utiliza varias estructuras de datos y conceptos de programación, como hashes, métodos, bucles y llamadas a métodos. Además, el código está diseñado para ser reutilizable, ya que el método `imprimir_persona` puede utilizarse para imprimir la información de cualquier persona.