```elixir
defmodule Biblioteca do
  # Función para crear una biblioteca.
  def crear_biblioteca() do
    # Crear una lista vacía de libros.
    libros = []

    # Crear una biblioteca con la lista de libros vacía.
    biblioteca = %Biblioteca{libros: libros}

    # Devolver la biblioteca.
    biblioteca
  end

  # Función para agregar un libro a la biblioteca.
  def agregar_libro(biblioteca, libro) do
    # Obtener la lista actual de libros.
    libros = biblioteca.libros

    # Agregar el libro a la lista de libros.
    libros = [libro | libros]

    # Crear una nueva biblioteca con la lista actualizada de libros.
    biblioteca = %Biblioteca{libros: libros}

    # Devolver la biblioteca actualizada.
    biblioteca
  end

  # Función para obtener un libro de la biblioteca por su título.
  def obtener_libro_por_titulo(biblioteca, titulo) do
    # Obtener la lista de libros.
    libros = biblioteca.libros

    # Buscar el libro en la lista por su título.
    libro = Enum.find(libros, fn libro -> libro.titulo == titulo end)

    # Devolver el libro encontrado.
    libro
  end

  # Función para eliminar un libro de la biblioteca por su título.
  def eliminar_libro_por_titulo(biblioteca, titulo) do
    # Obtener la lista de libros.
    libros = biblioteca.libros

    # Eliminar el libro de la lista por su título.
    libros = Enum.filter(libros, fn libro -> libro.titulo != titulo end)

    # Crear una nueva biblioteca con la lista actualizada de libros.
    biblioteca = %Biblioteca{libros: libros}

    # Devolver la biblioteca actualizada.
    biblioteca
  end
end

# Crear una biblioteca.
biblioteca = Biblioteca.crear_biblioteca()

# Agregar algunos libros a la biblioteca.
biblioteca = Biblioteca.agregar_libro(biblioteca, %Libro{titulo: "El Señor de los Anillos", autor: "J.R.R. Tolkien"})
biblioteca = Biblioteca.agregar_libro(biblioteca, %Libro{titulo: "Harry Potter y la Piedra Filosofal", autor: "J.K. Rowling"})
biblioteca = Biblioteca.agregar_libro(biblioteca, %Libro{titulo: "El Principito", autor: "Antoine de Saint-Exupéry"})

# Obtener un libro de la biblioteca por su título.
libro = Biblioteca.obtener_libro_por_titulo(biblioteca, "El Señor de los Anillos")

# Eliminar un libro de la biblioteca por su título.
biblioteca = Biblioteca.eliminar_libro_por_titulo(biblioteca, "Harry Potter y la Piedra Filosofal")

# Mostrar la lista de libros en la biblioteca.
Enum.each(biblioteca.libros, fn libro -> IO.puts(libro.titulo) end)
```

Explicación del código:

1. Definimos un módulo llamado `Biblioteca` que contiene funciones para crear, agregar, obtener y eliminar libros de la biblioteca.
2. La función `crear_biblioteca()` crea una biblioteca vacía.
3. La función `agregar_libro()` agrega un libro a la biblioteca.
4. La función `obtener_libro_por_titulo()` obtiene un libro de la biblioteca por su título.
5. La función `eliminar_libro_por_titulo()` elimina un libro de la biblioteca por su título.
6. Creamos una biblioteca utilizando la función `crear_biblioteca()`.
7. Agregamos algunos libros a la biblioteca utilizando la función `agregar_libro()`.
8. Obtenemos un libro de la biblioteca utilizando la función `obtener_libro_por_titulo()`.
9. Eliminamos un libro de la biblioteca utilizando la función `eliminar_libro_por_titulo()`.
10. Mostramos la lista de libros en la biblioteca.