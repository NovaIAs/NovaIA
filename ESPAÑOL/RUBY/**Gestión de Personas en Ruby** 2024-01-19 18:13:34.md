```ruby
# Objeto Persona con atributos nombre, edad y ciudad.
class Persona
  attr_accessor :nombre, :edad, :ciudad

  # Constructor de la clase Persona.
  def initialize(nombre, edad, ciudad)
    @nombre = nombre
    @edad = edad
    @ciudad = ciudad
  end

  # Método para obtener el nombre de la persona.
  def obtener_nombre
    @nombre
  end

  # Método para obtener la edad de la persona.
  def obtener_edad
    @edad
  end

  # Método para obtener la ciudad de la persona.
  def obtener_ciudad
    @ciudad
  end

  # Método para cambiar el nombre de la persona.
  def cambiar_nombre(nuevo_nombre)
    @nombre = nuevo_nombre
  end

  # Método para cambiar la edad de la persona.
  def cambiar_edad(nueva_edad)
    @edad = nueva_edad
  end

  # Método para cambiar la ciudad de la persona.
  def cambiar_ciudad(nueva_ciudad)
    @ciudad = nueva_ciudad
  end

  # Método para obtener una representación en cadena de la persona.
  def to_s
    "Nombre: #{@nombre}, Edad: #{@edad}, Ciudad: #{@ciudad}"
  end
end

# Clase ListaPersonas que contiene una lista de objetos Persona.
class ListaPersonas
  # Array para almacenar los objetos Persona.
  @personas = []

  # Método para agregar una persona a la lista.
  def agregar_persona(persona)
    @personas.push(persona)
  end

  # Método para obtener la lista de personas.
  def obtener_personas
    @personas
  end

  # Método para obtener el número de personas en la lista.
  def obtener_numero_personas
    @personas.length
  end

  # Método para eliminar una persona de la lista por su nombre.
  def eliminar_persona_por_nombre(nombre)
    @personas.delete_if { |persona| persona.obtener_nombre == nombre }
  end

  # Método para eliminar una persona de la lista por su edad.
  def eliminar_persona_por_edad(edad)
    @personas.delete_if { |persona| persona.obtener_edad == edad }
  end

  # Método para eliminar una persona de la lista por su ciudad.
  def eliminar_persona_por_ciudad(ciudad)
    @personas.delete_if { |persona| persona.obtener_ciudad == ciudad }
  end

  # Método para buscar una persona en la lista por su nombre.
  def buscar_persona_por_nombre(nombre)
    @personas.find { |persona| persona.obtener_nombre == nombre }
  end

  # Método para buscar una persona en la lista por su edad.
  def buscar_persona_por_edad(edad)
    @personas.find { |persona| persona.obtener_edad == edad }
  end

  # Método para buscar una persona en la lista por su ciudad.
  def buscar_persona_por_ciudad(ciudad)
    @personas.find { |persona| persona.obtener_ciudad == ciudad }
  end

  # Método para obtener una representación en cadena de la lista de personas.
  def to_s
    "Lista de Personas:\n" + @personas.join("\n")
  end
end

# Crear una lista de personas.
lista_personas = ListaPersonas.new

# Agregar algunas personas a la lista.
lista_personas.agregar_persona(Persona.new("Juan", 20, "Madrid"))
lista_personas.agregar_persona(Persona.new("María", 30, "Barcelona"))
lista_personas.agregar_persona(Persona.new("Pedro", 40, "Sevilla"))

# Obtener la lista de personas.
puts "Lista de Personas:"
puts lista_personas.obtener_personas

# Obtener el número de personas en la lista.
puts "Número de Personas:"
puts lista_personas.obtener_numero_personas

# Eliminar una persona de la lista por su nombre.
lista_personas.eliminar_persona_por_nombre("María")

# Obtener la lista de personas después de eliminar a María.
puts "Lista de Personas después de eliminar a María:"
puts lista_personas.obtener_personas

# Buscar una persona en la lista por su nombre.
persona_buscada = lista_personas.buscar_persona_por_nombre("Pedro")

# Obtener el nombre de la persona buscada.
puts "Nombre de la Persona Buscada:"
puts persona_buscada.obtener_nombre

# Cambiar el nombre de la persona buscada.
persona_buscada.cambiar_nombre("Pablo")

# Obtener la lista de personas después de cambiar el nombre de Pedro.
puts "Lista de Personas después de cambiar el nombre de Pedro:"
puts lista_personas.obtener_personas
```

Este código crea una clase Persona con los atributos nombre, edad y ciudad. También crea una clase ListaPersonas que contiene una lista de objetos Persona. El código agrega algunas personas a la lista, elimina a una persona por su nombre, busca a una persona por su nombre y cambia el nombre de una persona.