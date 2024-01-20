```elixir
# Módulo para manejar la gestión de usuarios
defmodule Usuarios do
  # Definimos una estructura para representar un usuario
  defstruct [:nombre, :apellido, :correo, :contraseña]

  # Función para crear un nuevo usuario
  def crear_usuario(nombre, apellido, correo, contraseña) do
    %Usuarios{
      nombre: nombre,
      apellido: apellido,
      correo: correo,
      contraseña: contraseña
    }
  end

  # Función para validar el login de un usuario
  def validar_login(correo, contraseña) do
    # Buscamos el usuario en la base de datos
    usuario = buscar_usuario_por_correo(correo)

    # Si el usuario no existe, devolvemos un error
    if usuario == nil do
      {:error, :usuario_no_existe}
    end

    # Si la contraseña no coincide, devolvemos un error
    if usuario.contraseña != contraseña do
      {:error, :contraseña_incorrecta}
    end

    # Si todo está correcto, devolvemos el usuario
    {:ok, usuario}
  end

  # Función para buscar un usuario por su correo
  def buscar_usuario_por_correo(correo) do
    # Aquí haríamos una consulta a la base de datos para buscar el usuario
    # Por simplicidad, devolvemos un usuario ficticio
    %Usuarios{
      nombre: "Juan",
      apellido: "García",
      correo: "juan.garcia@ejemplo.com",
      contraseña: "123456"
    }
  end
end

# Módulo para manejar la gestión de mensajes
defmodule Mensajes do
  # Definimos una estructura para representar un mensaje
  defstruct [:remitente, :destinatario, :asunto, :contenido]

  # Función para crear un nuevo mensaje
  def crear_mensaje(remitente, destinatario, asunto, contenido) do
    %Mensajes{
      remitente: remitente,
      destinatario: destinatario,
      asunto: asunto,
      contenido: contenido
    }
  end

  # Función para enviar un mensaje
  def enviar_mensaje(mensaje) do
    # Aquí haríamos una llamada a un servicio de correo electrónico para enviar el mensaje
    # Por simplicidad, devolvemos un mensaje de éxito
    {:ok, "Mensaje enviado correctamente"}
  end
end

# Módulo para manejar la gestión de contactos
defmodule Contactos do
  # Definimos una estructura para representar un contacto
  defstruct [:nombre, :apellido, :correo, :teléfono]

  # Función para crear un nuevo contacto
  def crear_contacto(nombre, apellido, correo, teléfono) do
    %Contactos{
      nombre: nombre,
      apellido: apellido,
      correo: correo,
      teléfono: teléfono
    }
  end

  # Función para añadir un contacto a la lista de contactos
  def añadir_contacto(contacto) do
    # Aquí haríamos una llamada a una base de datos para añadir el contacto
    # Por simplicidad, devolvemos un mensaje de éxito
    {:ok, "Contacto añadido correctamente"}
  end
end

# Módulo principal de la aplicación
defmodule Aplicacion do
  # Función principal de la aplicación
  def main() do
    # Creamos un usuario ficticio
    usuario = Usuarios.crear_usuario("Juan", "García", "juan.garcia@ejemplo.com", "123456")

    # Creamos un mensaje ficticio
    mensaje = Mensajes.crear_mensaje(usuario, "maria.perez@ejemplo.com", "Asunto del mensaje", "Contenido del mensaje")

    # Enviamos el mensaje
    Mensajes.enviar_mensaje(mensaje)

    # Creamos un contacto ficticio
    contacto = Contactos.crear_contacto("María", "Pérez", "maria.perez@ejemplo.com", "123456789")

    # Añadimos el contacto a la lista de contactos
    Contactos.añadir_contacto(contacto)

    # Imprimimos un mensaje de éxito
    IO.puts("Operaciones realizadas correctamente")
  end
end

# Ejecutamos la aplicación
Aplicacion.main()
```

Explicación del código:

* El código está estructurado en módulos, lo que ayuda a mantener el código organizado y modular.
* Se utilizan estructuras para representar los datos, lo que hace que el código sea más fácil de entender y mantener.
* Se utilizan funciones para realizar las diferentes operaciones, lo que hace que el código sea más reutilizable.
* Se utilizan llamadas a funciones para enviar mensajes de correo electrónico y añadir contactos a la base de datos, lo que hace que el código sea más fácil de probar y mantener.
* El código utiliza la función `IO.puts()` para imprimir mensajes en la consola, lo que ayuda a depurar el código y a obtener información sobre su ejecución.
* El código utiliza la función `main()` como punto de entrada de la aplicación, lo que permite ejecutar el código desde la consola.