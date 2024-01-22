```ruby
# Definir una clase "Persona" que represente a una persona con su nombre y edad
class Persona
  attr_accessor :nombre, :edad

  def initialize(nombre, edad)
    @nombre = nombre
    @edad = edad
  end

  def saludar
    puts "Hola, mi nombre es #{@nombre} y tengo #{@edad} años."
  end
end

# Crear una serie de instancias de "Persona" con diferentes características
persona1 = Persona.new("Juan", 20)
persona2 = Persona.new("María", 25)
persona3 = Persona.new("Pedro", 30)

# Almacenar todas las instancias de "Persona" en un array
personas = [persona1, persona2, persona3]

# Utilizar el método "each" para iterar sobre el array de personas y saludar a cada una
personas.each do |persona|
  persona.saludar
end

# Definir un método "mayor_de_edad?" para comprobar si una persona es mayor de edad
def mayor_de_edad?(persona)
  persona.edad >= 18
end

# Utilizar el método "select" para filtrar el array de personas y obtener solo las que son mayores de edad
personas_mayores_de_edad = personas.select do |persona|
  mayor_de_edad?(persona)
end

# Imprimir los nombres de las personas mayores de edad
puts "Personas mayores de edad:"
personas_mayores_de_edad.each do |persona|
  puts persona.nombre
end

# Crear un hash que represente una lista de compras con cantidades
lista_de_compras = {
  "manzanas" => 3,
  "peras" => 2,
  "plátanos" => 5
}

# Utilizar el método "each" para iterar sobre el hash de la lista de compras y mostrar cada artículo y su cantidad
puts "Lista de compras:"
lista_de_compras.each do |articulo, cantidad|
  puts "#{cantidad} #{articulo}"
end

# Utilizar el método "map" para crear un nuevo array con los artículos de la lista de compras
articulos = lista_de_compras.keys

# Imprimir el array de artículos
puts "Artículos en la lista de compras:"
puts articulos

# Utilizar el método "sort" para ordenar el array de artículos alfabéticamente
articulos_ordenados = articulos.sort

# Imprimir el array de artículos ordenados
puts "Artículos ordenados alfabéticamente:"
puts articulos_ordenados
```

Explicación del código:

1. Definimos una clase "Persona" que representa a una persona con su nombre y edad.
2. Creamos una serie de instancias de "Persona" con diferentes características y las almacenamos en un array.
3. Utilizamos el método "each" para iterar sobre el array de personas y saludar a cada una.
4. Definimos un método "mayor_de_edad?" para comprobar si una persona es mayor de edad.
5. Utilizamos el método "select" para filtrar el array de personas y obtener solo las que son mayores de edad.
6. Imprimimos los nombres de las personas mayores de edad.
7. Creamos un hash que representa una lista de compras con cantidades.
8. Utilizamos el método "each" para iterar sobre el hash de la lista de compras y mostrar cada artículo y su cantidad.
9. Utilizamos el método "map" para crear un nuevo array con los artículos de la lista de compras.
10. Imprimimos el array de artículos.
11. Utilizamos el método "sort" para ordenar el array de artículos alfabéticamente.
12. Imprimimos el array de artículos ordenados.