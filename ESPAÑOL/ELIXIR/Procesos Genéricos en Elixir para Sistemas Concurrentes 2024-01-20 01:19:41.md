```elixir
# Módulo que define un proceso genérico para manejar mensajes.

defmodule ManejadorDeMensajes do
  use GenServer

  # Función de callback que se llama cuando se inicia el proceso.
  def init(arg) do
    {:ok, arg}
  end

  # Función de callback que se llama cuando se recibe un mensaje.
  def handle_message(msg, state) do
    # Aquí se procesa el mensaje.
    {:noreply, state}
  end
end

# Módulo que define un cliente que puede enviar mensajes a un proceso.

defmodule ClienteDeMensajes do
  use GenServer

  # Función de callback que se llama cuando se inicia el proceso.
  def init(arg) do
    {:ok, arg}
  end

  # Función de callback que se llama cuando se recibe un mensaje.
  def handle_call(msg, from, state) do
    # Aquí se envía el mensaje al proceso.
    ManejadorDeMensajes.cast(msg)
    {:reply, :ok, state}
  end
end

# Creamos un proceso manejador de mensajes.
maneja_mensajes = GenServer.start_link(ManejadorDeMensajes, :ok)

# Creamos un proceso cliente de mensajes.
cliente_mensajes = GenServer.start_link(ClienteDeMensajes, :ok)

# Enviamos un mensaje al proceso manejador de mensajes.
GenServer.cast(cliente_mensajes, :hola)

# Esperamos una respuesta del proceso cliente de mensajes.
respuesta = GenServer.call(cliente_mensajes, :hola)

# Imprimimos la respuesta.
IO.puts respuesta
```

Este código crea un proceso genérico que puede manejar mensajes. El proceso se define en el módulo `ManejadorDeMensajes`, que utiliza el comportamiento `GenServer`. El proceso se inicia llamando a la función `GenServer.start_link/2`, que devuelve un PID (identificador de proceso).

El proceso cliente de mensajes se define en el módulo `ClienteDeMensajes`, que también utiliza el comportamiento `GenServer`. El proceso se inicia llamando a la función `GenServer.start_link/2`, que devuelve un PID.

Para enviar un mensaje al proceso manejador de mensajes, se utiliza la función `GenServer.cast/2`. Esta función envía un mensaje al proceso sin esperar una respuesta.

Para recibir una respuesta del proceso manejador de mensajes, se utiliza la función `GenServer.call/3`. Esta función envía un mensaje al proceso y espera una respuesta.

El código también incluye una función `IO.puts/1` que se utiliza para imprimir la respuesta del proceso cliente de mensajes.

Este código es un ejemplo de cómo se pueden utilizar los procesos genéricos en Elixir para crear sistemas concurrentes.