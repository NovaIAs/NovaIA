```elixir
# CREAR UN MÓDULO CON UN ÁMBITO PRIVADO
defmodule Contador do
  @initial_count 0
  def start_count() do
    spawn(fn -> count(@initial_count) end)
  end

  defp count(count) do
    send(self(), :tick)
    receive do
      :tick ->
        IO.puts "El conteo actual es: #{count}"
        count(count + 1)
      :stop ->
        IO.puts "Conteo detenido."
    end
  end
end

# OBTENER EL ID DEL PROCESO
pid = Contador.start_count()

# ENVIAR MENSAJES AL PROCESO
Process.send(pid, :tick, [])
Process.send(pid, :tick, [])
Process.send(pid, :stop, [])
```

Este código crea un módulo `Contador` con un ámbito privado, que contiene una función privada `count/1` y una función pública `start_count/0`.

La función `start_count/0` inicia un proceso que ejecuta la función `count/1` en un bucle. La función `count/1` recibe un contador como argumento y lo incrementa cada vez que recibe un mensaje `:tick`. Cuando recibe un mensaje `:stop`, detiene el bucle y termina el proceso.

Para utilizar el módulo, se obtiene el ID del proceso creado por la función `start_count/0` y se le envían mensajes `:tick` y `:stop` para incrementar el contador y detener el proceso, respectivamente.

Este código es complejo porque utiliza conceptos avanzados de Elixir, como ámbitos privados, procesos y envío de mensajes. Sin embargo, es un buen ejemplo de cómo se pueden utilizar estas características para crear programas concurriculares eficientes y escalables.