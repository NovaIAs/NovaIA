```ruby
# Definimos una clase llamada "Persona" que representa a una persona con nombre, edad y género.
class Persona
  # Método constructor de la clase "Persona"
  def initialize(nombre, edad, género)
    # Asignamos los valores de los parámetros a las variables de instancia correspondientes.
    @nombre = nombre
    @edad = edad
    @género = género
  end

  # Método para obtener el nombre de la persona.
  def obtener_nombre
    # Devolvemos el valor de la variable de instancia @nombre.
    @nombre
  end

  # Método para obtener la edad de la persona.
  def obtener_edad
    # Devolvemos el valor de la variable de instancia @edad.
    @edad
  end

  # Método para obtener el género de la persona.
  def obtener_género
    # Devolvemos el valor de la variable de instancia @género.
    @género
  end

  # Método para establecer el nombre de la persona.
  def establecer_nombre(nombre)
    # Asignamos el valor del parámetro "nombre" a la variable de instancia @nombre.
    @nombre = nombre
  end

  # Método para establecer la edad de la persona.
  def establecer_edad(edad)
    # Asignamos el valor del parámetro "edad" a la variable de instancia @edad.
    @edad = edad
  end

  # Método para establecer el género de la persona.
  def establecer_género(género)
    # Asignamos el valor del parámetro "género" a la variable de instancia @género.
    @género = género
  end

  # Método para obtener una representación en cadena de la persona.
  def to_s
    # Devolvemos una cadena con el nombre, la edad y el género de la persona.
    "Nombre: #{@nombre}, Edad: #{@edad}, Género: #{@género}"
  end
end

# Definimos una clase llamada "Familia" que representa a una familia con un conjunto de personas.
class Familia
  # Método constructor de la clase "Familia"
  def initialize(*personas)
    # Asignamos el conjunto de personas a la variable de instancia @personas.
    @personas = personas
  end

  # Método para añadir una persona a la familia.
  def añadir_persona(persona)
    # Añadimos la persona a la variable de instancia @personas.
    @personas << persona
  end

  # Método para eliminar una persona de la familia.
  def eliminar_persona(persona)
    # Eliminamos la persona de la variable de instancia @personas.
    @personas.delete(persona)
  end

  # Método para obtener una lista de las personas de la familia.
  def obtener_personas
    # Devolvemos la lista de personas.
    @personas
  end

  # Método para obtener el número de personas de la familia.
  def obtener_número_de_personas
    # Devolvemos el número de personas.
    @personas.size
  end

  # Método para obtener una representación en cadena de la familia.
  def to_s
    # Devolvemos una cadena con el número de personas y la lista de personas de la familia.
    "Número de personas: #{@personas.size}, Personas: #{@personas}"
  end
end

# Creamos una instancia de la clase "Persona" llamada "juan".
juan = Persona.new("Juan", 25, "Masculino")

# Creamos una instancia de la clase "Persona" llamada "maría".
maría = Persona.new("María", 23, "Femenino")

# Creamos una instancia de la clase "Familia" llamada "familia_pérez".
familia_pérez = Familia.new(juan, maría)

# Añadimos una persona a la familia_pérez.
familia_pérez.añadir_persona(Persona.new("Pedro", 18, "Masculino"))

# Obtenemos la lista de personas de la familia_pérez.
personas_familia_pérez = familia_pérez.obtener_personas

# Imprimimos la lista de personas de la familia_pérez.
puts "Personas de la familia Pérez:"
personas_familia_pérez.each do |persona|
  puts persona
end

# Obtenemos el número de personas de la familia_pérez.
número_personas_familia_pérez = familia_pérez.obtener_número_de_personas

# Imprimimos el número de personas de la familia_pérez.
puts "Número de personas en la familia Pérez: #{número_personas_familia_pérez}"