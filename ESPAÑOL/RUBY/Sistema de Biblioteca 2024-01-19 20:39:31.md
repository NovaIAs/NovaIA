```ruby
# Definir una clase para representar libros
class Libro
  # Atributos de la clase libro
  attr_accessor :titulo, :autor, :año_publicación, :precio

  # Constructor de la clase libro
  def initialize(titulo, autor, año_publicación, precio)
    @titulo = titulo
    @autor = autor
    @año_publicación = año_publicación
    @precio = precio
  end

  # Método para obtener la información del libro en formato de cadena
  def to_s
    "Título: #{@titulo}, Autor: #{@autor}, Año de publicación: #{@año_publicación}, Precio: #{@precio}"
  end
end

# Definir una clase para representar una biblioteca
class Biblioteca
  # Atributos de la clase biblioteca
  attr_accessor :nombre, :dirección, :libros

  # Constructor de la clase biblioteca
  def initialize(nombre, dirección)
    @nombre = nombre
    @dirección = dirección
    @libros = []
  end

  # Método para agregar un libro a la biblioteca
  def agregar_libro(libro)
    @libros << libro
  end

  # Método para obtener la información de la biblioteca en formato de cadena
  def to_s
    "Nombre: #{@nombre}, Dirección: #{@dirección}, Libros: #{@libros}"
  end
end

# Crear una instancia de la clase libro
libro1 = Libro.new("El Quijote", "Miguel de Cervantes", 1605, 10.0)

# Crear una instancia de la clase libro
libro2 = Libro.new("Cien años de soledad", "Gabriel García Márquez", 1967, 12.0)

# Crear una instancia de la clase biblioteca
biblioteca1 = Biblioteca.new("Biblioteca Nacional de España", "Paseo de Recoletos, 20")

# Agregar libros a la biblioteca
biblioteca1.agregar_libro(libro1)
biblioteca1.agregar_libro(libro2)

# Mostrar la información de la biblioteca en consola
puts biblioteca1

```

Explicación del código:

* Se define una clase `Libro` que representa a un libro con los atributos `titulo`, `autor`, `año_publicación` y `precio`.
* Se define una clase `Biblioteca` que representa a una biblioteca con los atributos `nombre`, `dirección` y `libros`.
* Se crean dos instancias de la clase `Libro`, `libro1` y `libro2`, y se asignan valores a sus atributos.
* Se crea una instancia de la clase `Biblioteca`, `biblioteca1`, y se asignan valores a sus atributos.
* Se agregan los libros `libro1` y `libro2` a la biblioteca `biblioteca1` usando el método `agregar_libro`.
* Se muestra la información de la biblioteca usando el método `to_s`, que se define en la clase `Biblioteca`.