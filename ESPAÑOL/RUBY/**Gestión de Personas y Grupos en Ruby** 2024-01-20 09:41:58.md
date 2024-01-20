```ruby
# Módulo para gestionar personas
module Persona
  # Clase que representa a una persona
  class Persona
    # Constructor de la clase
    def initialize(nombre, apellido, edad)
      @nombre = nombre
      @apellido = apellido
      @edad = edad
    end

    # Método que devuelve el nombre de la persona
    def nombre
      @nombre
    end

    # Método que devuelve el apellido de la persona
    def apellido
      @apellido
    end

    # Método que devuelve la edad de la persona
    def edad
      @edad
    end

    # Método que devuelve una cadena con el nombre y el apellido de la persona
    def nombre_completo
      "#{@nombre} #{@apellido}"
    end
  end

  # Clase que representa a un grupo de personas
  class Grupo
    # Constructor de la clase
    def initialize(nombre)
      @nombre = nombre
      @personas = []
    end

    # Método que añade una persona al grupo
    def añadir_persona(persona)
      @personas << persona
    end

    # Método que devuelve una lista de las personas del grupo
    def personas
      @personas
    end

    # Método que devuelve el número de personas del grupo
    def tamaño
      @personas.size
    end
  end
end

# Definición de una persona
persona1 = Persona.new("Juan", "García", 25)

# Definición de otra persona
persona2 = Persona.new("María", "López", 30)

# Definición de un grupo de personas
grupo1 = Persona::Grupo.new("Amigos")

# Añadir personas al grupo
grupo1.añadir_persona(persona1)
grupo1.añadir_persona(persona2)

# Obtener el número de personas del grupo
puts "El grupo \"#{grupo1.nombre}\" tiene #{grupo1.tamaño} personas."

# Obtener una lista de las personas del grupo
personas = grupo1.personas
personas.each do |persona|
  puts persona.nombre_completo
end
```

Explicación del código:

* El código comienza definiendo un módulo llamado `Persona`. Los módulos son utilizados para agrupar clases y métodos relacionados.
* Dentro del módulo `Persona`, se define una clase llamada `Persona`. Esta clase representa a una persona y tiene tres atributos: `nombre`, `apellido` y `edad`.
* La clase `Persona` tiene un constructor que inicializa los tres atributos de la persona.
* La clase `Persona` también tiene cuatro métodos: `nombre`, `apellido`, `edad` y `nombre_completo`. Estos métodos devuelven el valor de los atributos correspondientes o una cadena con el nombre y el apellido de la persona.
* A continuación, se define una segunda clase dentro del módulo `Persona`, llamada `Grupo`. Esta clase representa a un grupo de personas.
* La clase `Grupo` tiene un constructor que inicializa el nombre del grupo y una lista de personas vacía.
* La clase `Grupo` también tiene tres métodos: `añadir_persona`, `personas` y `tamaño`. El método `añadir_persona` añade una persona al grupo, el método `personas` devuelve una lista de las personas del grupo y el método `tamaño` devuelve el número de personas del grupo.
* Después de definir las clases `Persona` y `Grupo`, se crean dos objetos de la clase `Persona`, llamados `persona1` y `persona2`.
* A continuación, se crea un objeto de la clase `Grupo`, llamado `grupo1`.
* A continuación, se añaden los objetos `persona1` y `persona2` al objeto `grupo1`.
* Por último, se obtiene el número de personas del grupo `grupo1` y se muestra en la consola. También se obtiene una lista de las personas del grupo `grupo1` y se muestra en la consola.