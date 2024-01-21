```elixir
# Módulo principal de la aplicación.
defmodule MiAplicacion do

  # Función principal de la aplicación.
  def main() do

    # Crea un PipeLine de Elixir.
    Enum.pipe(%{nombre: "Juan", edad: 30},
      &Map.put(&1, :activo, true),
      &Map.merge(&1, %{direccion: "Calle Mayor, 123"}),
      fn mapa -> IO.inspect(mapa) end) |> :ok

    # Crea un Observer de Elixir.
    :observer.start(:sample_observer, [:info, :error])

    # Envía un mensaje al Observer.
    :info.log(:sample_observer, "Mensaje de información")

  end

end

# Módulo del Observer.
defmodule SampleObserver do

  use GenServer

  # Función de inicio del Observer.
  def start_link(_args) do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  # Función de manipulación de mensajes del Observer.
  def handle_info({:info, mensaje}, _state) do
    IO.puts("Mensaje de información recibido: #{mensaje}")
    {:noreply, _state}
  end

  def handle_info({:error, mensaje}, _state) do
    IO.puts("Mensaje de error recibido: #{mensaje}")
    {:noreply, _state}
  end

end

# Inicia la aplicación.
Application.start(:mi_aplicacion, [])

# Llama a la función principal de la aplicación.
MiAplicacion.main()
```

Este código es un programa complejo en Elixir que crea un PipeLine y un Observer.

El PipeLine es una tubería de datos que permite pasar datos a través de una serie de funciones. En este caso, el PipeLine se utiliza para añadir información a un mapa.

El Observer es un proceso que se ejecuta en segundo plano y escucha los mensajes que se envían a él. En este caso, el Observer se utiliza para registrar mensajes de información y error.

El código también inicia la aplicación y llama a la función principal de la aplicación.

Este es un ejemplo de código complejo en Elixir que demuestra algunas de las características avanzadas del lenguaje.