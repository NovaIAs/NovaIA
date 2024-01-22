```ruby
# Definición de una clase para representar una persona.
class Persona
  # Atributos de la clase.
  attr_accessor :nombre, :apellido, :edad, :sexo

  # Constructor de la clase.
  def initialize(nombre, apellido, edad, sexo)
    @nombre = nombre
    @apellido = apellido
    @edad = edad
    @sexo = sexo
  end

  # Método para obtener el nombre completo de la persona.
  def nombre_completo
    "#{@nombre} #{@apellido}"
  end

  # Método para obtener la edad de la persona.
  def edad
    @edad
  end

  # Método para obtener el sexo de la persona.
  def sexo
    @sexo
  end

  # Override del método to_s para devolver una representación de la persona como una cadena de texto.
  def to_s
    "Nombre: #{nombre_completo}, Edad: #{edad}, Sexo: #{sexo}"
  end
end

# Definición de una clase para representar una lista de personas.
class ListaPersonas
  # Atributos de la clase.
  attr_accessor :lista

  # Constructor de la clase.
  def initialize
    @lista = []
  end

  # Método para agregar una persona a la lista.
  def agregar(persona)
    @lista << persona
  end

  # Método para eliminar una persona de la lista por su índice.
  def eliminar(indice)
    @lista.delete_at(indice)
  end

  # Método para obtener una persona de la lista por su índice.
  def obtener_persona(indice)
    @lista[indice]
  end

  # Método para obtener el tamaño de la lista.
  def tamaño
    @lista.size
  end

  # Override del método to_s para devolver una representación de la lista como una cadena de texto.
  def to_s
    "Lista de Personas:\n#{@lista.each_with_index.map { |persona, i| "#{i + 1}. #{persona}" }.join("\n")}"
  end
end

# Creación de una lista de personas.
lista_personas = ListaPersonas.new

# Agregar personas a la lista.
lista_personas.agregar(Persona.new("Juan", "García", 25, "Masculino"))
lista_personas.agregar(Persona.new("María", "López", 30, "Femenino"))
lista_personas.agregar(Persona.new("Pedro", "Rodríguez", 35, "Masculino"))

# Obtener el tamaño de la lista.
puts "Tamaño de la lista: #{lista_personas.tamaño}"

# Obtener una persona de la lista por su índice.
puts "Persona en el índice 1: #{lista_personas.obtener_persona(1)}"

# Eliminar una persona de la lista por su índice.
lista_personas.eliminar(2)

# Obtener el tamaño de la lista después de eliminar una persona.
puts "Tamaño de la lista después de eliminar una persona: #{lista_personas.tamaño}"

# Imprimir la lista de personas.
puts lista_personas
```

Explicación del código:

- Se define una clase `Persona` para representar a una persona con atributos para su nombre, apellido, edad y sexo.
- Se define una clase `ListaPersonas` para representar una lista de personas con métodos para agregar, eliminar y obtener personas de la lista.
- Se crea una instancia de la clase `ListaPersonas` y se agregan varias personas a la lista.
- Se obtiene el tamaño de la lista y se muestra en la consola.
- Se obtiene una persona de la lista por su índice y se muestra en la consola.
- Se elimina una persona de la lista por su índice.
- Se obtiene nuevamente el tamaño de la lista después de eliminar una persona y se muestra en la consola.
- Se imprime la lista de personas en la consola.