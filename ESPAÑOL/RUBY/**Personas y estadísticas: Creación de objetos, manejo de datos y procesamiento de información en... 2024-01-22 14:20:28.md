```ruby
# Definición de una clase Persona con atributos y métodos
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  def saludar
    "Hola, mi nombre es #{@nombre} y tengo #{@edad} años. Soy #{@ocupacion}."
  end
end

# Creación de tres objetos de la clase Persona
persona1 = Persona.new("Juan", 25, "Ingeniero de Software")
persona2 = Persona.new("María", 30, "Médica")
persona3 = Persona.new("Pedro", 40, "Profesor")

# Almacenamiento de los objetos en un array
personas = [persona1, persona2, persona3]

# Iteración sobre el array de personas para imprimir el saludo de cada una
personas.each do |persona|
  puts persona.saludar
end

# Creación de un hash con los nombres de las personas como claves y sus edades como valores
edades = {}
personas.each do |persona|
  edades[persona.nombre] = persona.edad
end

# Impresión de las entradas del hash
edades.each do |nombre, edad|
  puts "#{nombre} tiene #{edad} años."
end

# Creación de un bloque para calcular el promedio de las edades de las personas
promedio_edades = personas.sum { |persona| persona.edad } / personas.length

# Impresión del promedio de las edades
puts "El promedio de las edades es #{promedio_edades} años."

# Creación de un método para filtrar las personas por ocupación
def filtrar_por_ocupacion(personas, ocupacion)
  personas.select { |persona| persona.ocupacion == ocupacion }
end

# Obtención de las personas que son Ingenieros de Software
ingenieros_software = filtrar_por_ocupacion(personas, "Ingeniero de Software")

# Impresión de los nombres de los Ingenieros de Software
ingenieros_software.each do |ingeniero|
  puts ingeniero.nombre
end
```

Este código crea una clase `Persona` con atributos `nombre`, `edad` y `ocupación`, y métodos para saludar e inicializar el objeto.

Luego, crea tres objetos de la clase `Persona` y los almacena en un array.

A continuación, crea un hash con los nombres de las personas como claves y sus edades como valores.

Luego, crea un bloque para calcular el promedio de las edades de las personas.

Después, crea un método para filtrar las personas por ocupación.

Finalmente, obtiene las personas que son Ingenieros de Software y las imprime.