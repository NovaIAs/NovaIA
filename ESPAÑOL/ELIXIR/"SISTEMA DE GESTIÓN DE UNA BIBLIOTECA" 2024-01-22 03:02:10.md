```elixir
defmodule Biblioteca do
  @moduledoc """
  Este módulo es parte del sistema de Gestión de una Biblioteca.
  Contiene las funciones necesarias para gestionar los libros,
  los autores, los usuarios y los préstamos.
  """

  defstruct [:libros, :autores, :usuarios, :prestamos]

  @spec crear_biblioteca() :: Biblioteca
  def crear_biblioteca() do
    %Biblioteca{libros: [], autores: [], usuarios: [], prestamos: []}
  end

  @spec agregar_libro(biblioteca :: Biblioteca, libro :: Libro) :: Biblioteca
  def agregar_libro(biblioteca, libro) do
    %{biblioteca | libros: [libro | biblioteca.libros]}
  end

  @spec agregar_autor(biblioteca :: Biblioteca, autor :: Autor) :: Biblioteca
  def agregar_autor(biblioteca, autor) do
    %{biblioteca | autores: [autor | biblioteca.autores]}
  end

  @spec agregar_usuario(biblioteca :: Biblioteca, usuario :: Usuario) :: Biblioteca
  def agregar_usuario(biblioteca, usuario) do
    %{biblioteca | usuarios: [usuario | biblioteca.usuarios]}
  end

  @spec agregar_prestamo(biblioteca :: Biblioteca, prestamo :: Prestamo) :: Biblioteca
  def agregar_prestamo(biblioteca, prestamo) do
    %{biblioteca | prestamos: [prestamo | biblioteca.prestamos]}
  end

  @spec encontrar_libro_por_titulo(biblioteca :: Biblioteca, titulo :: String) :: Libro | nil
  def encontrar_libro_por_titulo(biblioteca, titulo) do
    Enum.find(biblioteca.libros, &(&1.titulo == titulo))
  end

  @spec encontrar_autor_por_nombre(biblioteca :: Biblioteca, nombre :: String) :: Autor | nil
  def encontrar_autor_por_nombre(biblioteca, nombre) do
    Enum.find(biblioteca.autores, &(&1.nombre == nombre))
  end

  @spec encontrar_usuario_por_nombre(biblioteca :: Biblioteca, nombre :: String) :: Usuario | nil
  def encontrar_usuario_por_nombre(biblioteca, nombre) do
    Enum.find(biblioteca.usuarios, &(&1.nombre == nombre))
  end

  @spec encontrar_prestamo_por_id(biblioteca :: Biblioteca, id :: Integer) :: Prestamo | nil
  def encontrar_prestamo_por_id(biblioteca, id) do
    Enum.find(biblioteca.prestamos, &(&1.id == id))
  end

  @spec prestar_libro(biblioteca :: Biblioteca, libro :: Libro, usuario :: Usuario) :: Biblioteca
  def prestar_libro(biblioteca, libro, usuario) do
    prestamo = %Prestamo{id: biblioteca.prestamos |> length() + 1,
                          libro: libro,
                          usuario: usuario,
                          fecha_prestamo: Date.utc_today(),
                          fecha_devolucion: Date.utc_today() + 14}

    agregar_prestamo(biblioteca, prestamo)
  end

  @spec devolver_libro(biblioteca :: Biblioteca, prestamo :: Prestamo) :: Biblioteca
  def devolver_libro(biblioteca, prestamo) do
    %{biblioteca | prestamos: Enum.filter(biblioteca.prestamos, &(&1.id != prestamo.id))}
  end
end

defmodule Libro do
  @moduledoc """
  Este módulo representa un libro en el sistema de Gestión de una Biblioteca.
  Contiene los atributos necesarios para describir un libro,
  como el título, el autor, el año de publicación y el género.
  """

  defstruct [:titulo, :autor, :año_publicacion, :genero]

  @spec crear_libro(titulo :: String, autor :: Autor, año_publicacion :: Integer, genero :: String) :: Libro
  def crear_libro(titulo, autor, año_publicacion, genero) do
    %Libro{titulo: titulo, autor: autor, año_publicacion: año_publicacion, genero: genero}
  end
end

defmodule Autor do
  @moduledoc """
  Este módulo representa un autor en el sistema de Gestión de una Biblioteca.
  Contiene los atributos necesarios para describir un autor,
  como el nombre, la nacionalidad y el género literario.
  """

  defstruct [:nombre, :nacionalidad, :genero_literario]

  @spec crear_autor(nom