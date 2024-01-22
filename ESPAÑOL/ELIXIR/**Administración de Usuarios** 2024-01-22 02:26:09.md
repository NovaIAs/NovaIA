```elixir
# Módulo para manejo de datos
defmodule Datos do
  @nombre_archivo "datos.txt"

  # Función para leer datos de un archivo
  def leer_datos() do
    case File.read(@nombre_archivo) do
      {:ok, datos} -> datos
      {:error, _} -> ""
    end
  end

  # Función para escribir datos en un archivo
  def escribir_datos(datos) do
    File.write(@nombre_archivo, datos)
  end
end

# Módulo para manejo de usuarios
defmodule Usuarios do
  # Lista con los usuarios registrados
  @usuarios [%{nombre: "Juan", edad: 25},
             %{nombre: "María", edad: 30}]

  # Función para agregar un usuario
  def agregar_usuario(nombre, edad) do
    nuevo_usuario = %{nombre: nombre, edad: edad}
    @usuarios ++ [nuevo_usuario]
  end

  # Función para obtener la lista de usuarios
  def obtener_usuarios() do
    @usuarios
  end
end

# Módulo para manejo de la interfaz de usuario
defmodule Interfaz do
  # Función para mostrar el menú
  def mostrar_menu() do
    IO.puts("1. Agregar usuario")
    IO.puts("2. Ver lista de usuarios")
    IO.puts("3. Salir")
  end

  # Función para obtener la opción seleccionada
  def obtener_opcion() do
    IO.gets("Elige una opción: ") |> String.trim()
  end
end

# Función principal
defmodule Main do
  def main() do
    loop()
  end

  # Función para el bucle principal de la aplicación
  def loop() do
    Interfaz.mostrar_menu()
    opcion = Interfaz.obtener_opcion()

    case opcion do
      "1" -> agregar_usuario()
      "2" -> mostrar_usuarios()
      "3" -> salir()
      _ ->
        IO.puts("Opción inválida. Inténtalo de nuevo.")
        loop()
    end
  end

  # Función para agregar un usuario
  def agregar_usuario() do
    IO.puts("Introduce el nombre:")
    nombre = IO.gets() |> String.trim()

    IO.puts("Introduce la edad:")
    edad = IO.gets() |> String.to_integer()

    Usuarios.agregar_usuario(nombre, edad)

    IO.puts("Usuario agregado con éxito.")

    loop()
  end

  # Función para mostrar la lista de usuarios
  def mostrar_usuarios() do
    usuarios = Usuarios.obtener_usuarios()

    if usuarios == [] do
      IO.puts("No hay usuarios registrados.")
    else
      IO.puts("Lista de usuarios:")
      Enum.each(usuarios, fn usuario ->
        IO.puts("Nombre: #{usuario.nombre}, Edad: #{usuario.edad}")
      end)
    end

    loop()
  end

  # Función para salir de la aplicación
  def salir() do
    IO.puts("Saliendo de la aplicación...")
    System.halt(0)
  end
end

# Iniciar la aplicación
Main.main()
```

Explicación del código:

* El módulo `Datos` se utiliza para manejar la lectura y escritura de datos en un archivo.
* El módulo `Usuarios` se utiliza para manejar la lista de usuarios registrados.
* El módulo `Interfaz` se utiliza para mostrar el menú y obtener la opción seleccionada.
* El módulo `Main` contiene la función principal `main()` y el bucle principal `loop()`, que se encarga de mostrar el menú, obtener la opción seleccionada y llamar a las funciones correspondientes.
* La función `agregar_usuario()` se utiliza para añadir un nuevo usuario a la lista.
* La función `mostrar_usuarios()` se utiliza para mostrar la lista de usuarios registrados.
* La función `salir()` se utiliza para salir de la aplicación.

Este código es un ejemplo de una aplicación sencilla en Elixir que utiliza varios módulos para organizar el código y hacer que sea más legible y mantenible.