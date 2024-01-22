```elixir
# Definición de un módulo "Gestión de Biblioteca"
defmodule GestionBiblioteca do

  # Definición de una estructura "Libro" para almacenar información sobre un libro
  defstruct [:titulo, :autor, :año_publicacion, :paginas]

  # Definición de una función "crear_libro/1" para crear un nuevo libro dada su información
  def crear_libro(titulo, autor, año_publicacion, paginas) do
    %Libro{
      titulo: titulo,
      autor: autor,
      año_publicacion: año_publicacion,
      paginas: paginas
    }
  end

  # Definición de una función "añadir_libro/2" para añadir un libro a una lista de libros
  def añadir_libro(libro, biblioteca) do
    [libro | biblioteca]
  end

  # Definición de una función "eliminar_libro/2" para eliminar un libro de una lista de libros
  def eliminar_libro(libro, biblioteca) do
    Enum.filter(biblioteca, fn(l) -> l != libro end)
  end

  # Definición de una función "buscar_libro/2" para buscar un libro en una lista de libros por su título y autor
  def buscar_libro(titulo, autor, biblioteca) do
    Enum.find(biblioteca, fn(l) -> l.titulo == titulo and l.autor == autor end)
  end

  # Definición de una función "ordenar_biblioteca/1" para ordenar una lista de libros por año de publicación y páginas
  def ordenar_biblioteca(biblioteca) do
    Enum.sort_by(biblioteca, fn(l) -> {l.año_publicacion, l.paginas} end)
  end

  # Definición de una función "imprimir_biblioteca/1" para imprimir una lista de libros en una consola
  def imprimir_biblioteca(biblioteca) do
    Enum.each(biblioteca, fn(l) -> IO.puts "#{l.titulo} - #{l.autor} - #{l.año_publicacion} - #{l.paginas}" end)
  end
end

# Uso del módulo "Gestión de Biblioteca"
biblioteca = []

# Crear algunos libros
libro1 = GestionBiblioteca.crear_libro("El Quijote", "Miguel de Cervantes", 1605, 1023)
libro2 = GestionBiblioteca.crear_libro("Cien años de soledad", "Gabriel García Márquez", 1967, 417)

# Añadir los libros a la biblioteca
biblioteca = GestionBiblioteca.añadir_libro(libro1, biblioteca)
biblioteca = GestionBiblioteca.añadir_libro(libro2, biblioteca)

# Buscar un libro por su título y autor
libro_buscado = GestionBiblioteca.buscar_libro("El Quijote", "Miguel de Cervantes", biblioteca)

# Ordenar la biblioteca por año de publicación y páginas
biblioteca_ordenada = GestionBiblioteca.ordenar_biblioteca(biblioteca)

# Imprimir la biblioteca en una consola
GestionBiblioteca.imprimir_biblioteca(biblioteca_ordenada)

# Ejemplo de uso de la biblioteca
IO.puts("Libro buscado:")
IO.inspect(libro_buscado)
```

Explicación del código:

* El módulo `GestionBiblioteca` define una estructura `Libro` para representar un libro con campos como el título, el autor, el año de publicación y el número de páginas.
* También define varias funciones para trabajar con libros y una biblioteca de libros.
* En el uso del módulo, se crean dos libros y se añaden a una biblioteca.
* Luego se busca un libro por su título y autor, se ordena la biblioteca por año de publicación y páginas, y se imprime la biblioteca ordenada en una consola.
* El módulo `GestionBiblioteca` proporciona una interfaz completa para trabajar con libros y una biblioteca de libros, y se puede utilizar para construir una aplicación más compleja para gestionar una biblioteca.