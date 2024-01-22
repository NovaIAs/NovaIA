```ruby
# Módulo para manejar la base de datos
require 'sqlite3'

# Clase para representar un libro
class Libro
  # Atributos del libro
  attr_accessor :id, :titulo, :autor, :precio, :stock

  # Constructor del libro
  def initialize(id, titulo, autor, precio, stock)
    @id = id
    @titulo = titulo
    @autor = autor
    @precio = precio
    @stock = stock
  end

  # Método para guardar el libro en la base de datos
  def save
    db = SQLite3::Database.new "libreria.db"
    db.execute "INSERT INTO libros (titulo, autor, precio, stock) VALUES (?, ?, ?, ?)", [@titulo, @autor, @precio, @stock]
    db.close
  end

  # Método para actualizar el libro en la base de datos
  def update
    db = SQLite3::Database.new "libreria.db"
    db.execute "UPDATE libros SET titulo = ?, autor = ?, precio = ?, stock = ? WHERE id = ?", [@titulo, @autor, @precio, @stock, @id]
    db.close
  end

  # Método para eliminar el libro de la base de datos
  def delete
    db = SQLite3::Database.new "libreria.db"
    db.execute "DELETE FROM libros WHERE id = ?", [@id]
    db.close
  end

  # Método para obtener todos los libros de la base de datos
  def self.all
    db = SQLite3::Database.new "libreria.db"
    libros = db.execute "SELECT * FROM libros"
    db.close
    libros.map { |libro| Libro.new(libro[0], libro[1], libro[2], libro[3], libro[4]) }
  end

  # Método para obtener un libro por su ID
  def self.find(id)
    db = SQLite3::Database.new "libreria.db"
    libro = db.execute "SELECT * FROM libros WHERE id = ?", [id].first
    db.close
    Libro.new(libro[0], libro[1], libro[2], libro[3], libro[4])
  end
end

# Código para crear la tabla de libros en la base de datos
db = SQLite3::Database.new "libreria.db"
db.execute "CREATE TABLE IF NOT EXISTS libros (id INTEGER PRIMARY KEY, titulo TEXT, autor TEXT, precio REAL, stock INTEGER)"
db.close

# Código para crear algunos libros de ejemplo
libro1 = Libro.new(nil, "El Quijote", "Miguel de Cervantes Saavedra", 12.95, 5)
libro2 = Libro.new(nil, "Cien años de soledad", "Gabriel García Márquez", 15.95, 3)
libro3 = Libro.new(nil, "El señor de los anillos", "J.R.R. Tolkien", 24.95, 2)

# Guardamos los libros en la base de datos
libro1.save
libro2.save
libro3.save

# Obtenemos todos los libros de la base de datos
libros = Libro.all

# Imprimimos el título de cada libro
libros.each do |libro|
  puts libro.titulo
end

# Obtenemos un libro por su ID
libro = Libro.find(2)

# Imprimimos el título del libro
puts libro.titulo
```

Explicación del código:

* El código crea una clase `Libro` que representa un libro. Esta clase tiene atributos para el título, el autor, el precio y el stock. También tiene métodos para guardar, actualizar y eliminar libros de la base de datos, así como métodos para obtener todos los libros y un libro por su ID.
* El código utiliza la biblioteca `sqlite3` para conectarse a la base de datos SQLite e interactuar con ella.
* El código crea una tabla `libros` en la base de datos con los campos `id`, `titulo`, `autor`, `precio` y `stock`.
* El código crea algunos libros de ejemplo y los guarda en la base de datos.
* El código obtiene todos los libros de la base de datos y los imprime.
* El código obtiene un libro por su ID y lo imprime.