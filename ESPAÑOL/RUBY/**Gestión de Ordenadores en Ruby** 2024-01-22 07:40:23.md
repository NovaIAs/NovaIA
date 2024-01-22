```ruby
# Definimos una clase llamada "Ordenador" que representa un ordenador.
class Ordenador
  # La clase "Ordenador" tiene dos atributos: "marca" y "modelo".
  attr_accessor :marca, :modelo

  # El método "__init__" se ejecuta cuando se crea un nuevo objeto de la clase "Ordenador".
  def initialize(marca, modelo)
    # Asignamos los valores de los parámetros "marca" y "modelo" a los atributos correspondientes del objeto.
    self.marca = marca
    self.modelo = modelo
  end

  # El método "to_s" devuelve una cadena de texto que representa el objeto "Ordenador".
  def to_s
    # La cadena de texto incluye la marca y el modelo del ordenador.
    "Ordenador de marca #{marca} y modelo #{modelo}"
  end
end

# Definimos una clase llamada "ListaOrdenadores" que representa una lista de ordenadores.
class ListaOrdenadores
  # La clase "ListaOrdenadores" tiene un atributo llamado "ordenadores" que es una lista de objetos de la clase "Ordenador".
  attr_accessor :ordenadores

  # El método "__init__" se ejecuta cuando se crea un nuevo objeto de la clase "ListaOrdenadores".
  def initialize()
    # Inicializamos la lista de ordenadores como una lista vacía.
    self.ordenadores = []
  end

  # El método "añadir_ordenador" añade un ordenador a la lista.
  def añadir_ordenador(ordenador)
    # Añadimos el ordenador a la lista.
    ordenadores.append(ordenador)
  }

  # El método "eliminar_ordenador" elimina un ordenador de la lista.
  def eliminar_ordenador(ordenador)
    # Eliminamos el ordenador de la lista.
    ordenadores.delete(ordenador)
  }

  # El método "buscar_ordenador" busca un ordenador en la lista por su marca y modelo.
  def buscar_ordenador(marca, modelo)
    # Iteramos sobre la lista de ordenadores.
    for ordenador in ordenadores
      # Si la marca y el modelo del ordenador coinciden con los parámetros de búsqueda, devolvemos el ordenador.
      if ordenador.marca == marca && ordenador.modelo == modelo
        return ordenador
      end
    end

    # Si no encontramos el ordenador, devolvemos "None".
    return nil
  }

  # El método "to_s" devuelve una cadena de texto que representa la lista de ordenadores.
  def to_s
    # La cadena de texto incluye la marca y el modelo de cada ordenador de la lista.
    "Lista de ordenadores:\n" + ordenadores.join("\n")
  end
end

# Creamos una lista de ordenadores.
lista_ordenadores = ListaOrdenadores.new()

# Añadimos algunos ordenadores a la lista.
lista_ordenadores.añadir_ordenador(Ordenador.new("Apple", "MacBook Pro"))
lista_ordenadores.añadir_ordenador(Ordenador.new("Microsoft", "Surface Pro"))
lista_ordenadores.añadir_ordenador(Ordenador.new("Dell", "XPS 13"))

# Eliminamos un ordenador de la lista.
lista_ordenadores.eliminar_ordenador(Ordenador.new("Apple", "MacBook Pro"))

# Buscamos un ordenador en la lista por su marca y modelo.
ordenador_buscado = lista_ordenadores.buscar_ordenador("Microsoft", "Surface Pro")

# Imprimimos la lista de ordenadores.
puts lista_ordenadores

# Imprimimos el ordenador buscado.
puts ordenador_buscado
```

Explicación del código:

* La clase "Ordenador" representa un ordenador. Tiene dos atributos: "marca" y "modelo". El método "__init__" se ejecuta cuando se crea un nuevo objeto de la clase "Ordenador" y asigna los valores de los parámetros "marca" y "modelo" a los atributos correspondientes del objeto. El método "to_s" devuelve una cadena de texto que representa el objeto "Ordenador".


* La clase "ListaOrdenadores" representa una lista de ordenadores. Tiene un atributo llamado "ordenadores" que es una lista de objetos de la clase "Ordenador". El método "__init__" se ejecuta cuando se crea un nuevo objeto de la clase "ListaOrdenadores" e inicializa la lista de ordenadores como una lista vacía. El método "añadir_ordenador" añade un ordenador a la lista. El método "eliminar_ordenador" elimina un ordenador de la lista. El método "buscar_ordenador" busca un ordenador en la lista por su marca y modelo. El método "to_s" devuelve una cadena de texto que representa la lista de ordenadores.


* Creamos una lista de ordenadores y añadimos algunos ordenadores a la lista.


* Eliminamos un ordenador de la lista.


* Buscamos un ordenador en la lista por su marca y modelo.


* Imprimimos la lista de ordenadores y el ordenador buscado.