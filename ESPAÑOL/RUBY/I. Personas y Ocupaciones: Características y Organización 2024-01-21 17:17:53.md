```ruby
# 1. Crea una clase llamada "Persona" con los atributos "nombre", "edad" y "ocupación".
class Persona
  attr_accessor :nombre, :edad, :ocupacion

  # Este método inicializa los atributos de la persona.
  def initialize(nombre, edad, ocupacion)
    @nombre = nombre
    @edad = edad
    @ocupacion = ocupacion
  end

  # Este método devuelve una cadena de texto con la información de la persona.
  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Ocupación: #{@ocupacion}"
  end
end

# 2. Crea un array de personas.
personas = [
  Persona.new("Juan", 25, "Estudiante"),
  Persona.new("María", 30, "Profesora"),
  Persona.new("Pedro", 35, "Ingeniero"),
  Persona.new("Ana", 40, "Médica")
]

# 3. Recorre el array de personas e imprime su información.
personas.each do |persona|
  puts persona
end

# 4. Filtra el array de personas y devuelve un nuevo array con las personas que tengan más de 30 años.
personas_mayores_de_30 = personas.select do |persona|
  persona.edad > 30
end

# 5. Imprime la información de las personas mayores de 30 años.
puts "Personas mayores de 30 años:"
personas_mayores_de_30.each do |persona|
  puts persona
end

# 6. Ordena el array de personas por edad.
personas_ordenadas_por_edad = personas.sort_by { |persona| persona.edad }

# 7. Imprime la información de las personas ordenadas por edad.
puts "Personas ordenadas por edad:"
personas_ordenadas_por_edad.each do |persona|
  puts persona
end

# 8. Crea un hash con las ocupaciones de las personas como claves y un array de personas con esa ocupación como valores.
ocupaciones = {}
personas.each do |persona|
  if ocupaciones[persona.ocupacion]
    ocupaciones[persona.ocupacion] << persona
  else
    ocupaciones[persona.ocupacion] = [persona]
  end
end

# 9. Imprime la información de las ocupaciones y las personas que las desempeñan.
puts "Ocupaciones y personas que las desempeñan:"
ocupaciones.each do |ocupacion, personas|
  puts "Ocupación: #{ocupacion}"
  puts "Personas:"
  personas.each do |persona|
    puts "\t- #{persona}"
  end
end
```

**Explicación del código:**

1. La clase `Persona` es una clase simple que define los atributos de una persona: nombre, edad y ocupación.
2. El array `personas` es un array de objetos `Persona`.
3. El bucle `each` recorre el array `personas` e imprime la información de cada persona.
4. El método `select` filtra el array `personas` y devuelve un nuevo array con las personas que cumplen la condición especificada. En este caso, la condición es que la edad de la persona sea mayor que 30.
5. El bucle `each` recorre el array `personas_mayores_de_30` e imprime la información de cada persona.
6. El método `sort_by` ordena el array `personas` según la edad de las personas.
7. El bucle `each` recorre el array `personas_ordenadas_por_edad` e imprime la información de cada persona.
8. El bucle `each` recorre el array `personas` y agrega cada persona al hash `ocupaciones` según su ocupación.
9. El bucle `each` recorre el hash `ocupaciones` e imprime la información de cada ocupación y las personas que la desempeñan.