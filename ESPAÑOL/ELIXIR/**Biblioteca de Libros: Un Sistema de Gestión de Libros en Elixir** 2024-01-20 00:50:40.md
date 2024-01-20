```elixir
# Definimos un módulo para organizar nuestro código.
defmodule Biblioteca do
  # Definimos una lista de libros.
  @libros [
    %{titulo: "El Quijote", autor: "Miguel de Cervantes"},
    %{titulo: "Cien años de soledad", autor: "Gabriel García Márquez"},
    %{titulo: "El señor de los anillos", autor: "J. R. R. Tolkien"},
  ]

  # Definimos una función para obtener todos los libros.
  def obtener_libros() do
    @libros
  end

  # Definimos una función para obtener un libro por su título.
  def obtener_libro_por_titulo(titulo) do
    Enum.find(@libros, &(&1.titulo == titulo))
  end

  # Definimos una función para agregar un libro a la lista.
  def agregar_libro(libro) do
    updated_books = [@libros | libro]
    {:ok, updated_books}
  end

  # Definimos una función para eliminar un libro de la lista.
  def eliminar_libro(titulo) do
    updated_books = Enum.filter(@libros, &(&1.titulo != titulo))
    {:ok, updated_books}
  end
end

# Definimos una función para imprimir los libros en la biblioteca.
def imprimir_libros(libros) do
  Enum.each(libros, fn libro ->
    IO.puts "#{libro.titulo} - #{libro.autor}"
  end)
end

# Obtenemos todos los libros de la biblioteca.
libros = Biblioteca.obtener_libros()

# Imprimimos los libros en la biblioteca.
imprimir_libros(libros)

# Añadimos un nuevo libro a la biblioteca.
nuevo_libro = %{titulo: "El principito", autor: "Antoine de Saint-Exupéry"}
Biblioteca.agregar_libro(nuevo_libro)

# Obtenemos todos los libros de la biblioteca.
libros = Biblioteca.obtener_libros()

# Imprimimos los libros en la biblioteca.
imprimir_libros(libros)

# Eliminamos un libro de la biblioteca.
titulo_a_eliminar = "Cien años de soledad"
Biblioteca.eliminar_libro(titulo_a_eliminar)

# Obtenemos todos los libros de la biblioteca.
libros = Biblioteca.obtener_libros()

# Imprimimos los libros en la biblioteca.
imprimir_libros(libros)
```

Explicación del código:

1. Creamos un módulo llamado `Biblioteca` para organizar nuestro código.
2. Definimos una lista de libros llamada `@libros` que contiene información sobre el título y el autor para cada libro.
3. Definimos una función llamada `obtener_libros()` que devuelve todos los libros de la biblioteca.
4. Definimos una función llamada `obtener_libro_por_titulo(titulo)` que devuelve el libro con el título especificado, si existe.
5. Definimos una función llamada `agregar_libro(libro)` que agrega un nuevo libro a la biblioteca.
6. Definimos una función llamada `eliminar_libro(titulo)` que elimina el libro con el título especificado de la biblioteca.
7. Definimos una función llamada `imprimir_libros(libros)` que imprime los libros en la consola.
8. Obtenemos todos los libros de la biblioteca y los imprimimos en la consola.
9. Añadimos un nuevo libro a la biblioteca e imprimimos los libros en la consola.
10. Eliminamos un libro de la biblioteca e imprimimos los libros en la consola.