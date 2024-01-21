```elixir
# Módulo para gestionar una lista de tareas
defmodule ListaTareas do
  # Definir un nuevo tipo de datos para representar una tarea
  defstruct tarea: "", completada: false

  # Función para crear una nueva tarea
  def nueva_tarea(tarea) do
    %ListaTareas{tarea: tarea}
  end

  # Función para marcar una tarea como completada
  def marcar_completada(tarea) do
    %ListaTareas{tarea: tarea.tarea, completada: true}
  end

  # Función para obtener la lista de tareas
  def obtener_lista() do
    [
      nueva_tarea("Comprar leche"),
      nueva_tarea("Pasear al perro"),
      nueva_tarea("Limpiar el baño")
    ]
  end

  # Función para mostrar la lista de tareas en pantalla
  def mostrar_lista(lista) do
    Enum.each(lista, fn tarea ->
      IO.puts "#{tarea.tarea} - #{if tarea.completada, do: "Completada", else: "Pendiente"}"
    end)
  end
end

# Crear una lista de tareas
lista = ListaTareas.obtener_lista()

# Mostrar la lista de tareas en pantalla
ListaTareas.mostrar_lista(lista)

# Marcar la primera tarea como completada
lista = [ListaTareas.marcar_completada(Enum.at(lista, 0)) | Enum.drop(lista, 1)]

# Mostrar la lista de tareas en pantalla nuevamente
ListaTareas.mostrar_lista(lista)
```

Este código define un módulo Elixir llamado `ListaTareas` que se utiliza para gestionar una lista de tareas. El módulo contiene las siguientes funciones:

* `nueva_tarea(tarea)`: crea una nueva tarea con el nombre proporcionado.
* `marcar_completada(tarea)`: marca una tarea como completada.
* `obtener_lista()`: obtiene la lista de tareas.
* `mostrar_lista(lista)`: muestra la lista de tareas en pantalla.

El código principal crea una lista de tareas, la muestra en pantalla, marca la primera tarea como completada y vuelve a mostrar la lista en pantalla.

El código está escrito en Elixir, un lenguaje de programación funcional y concurrente que se utiliza para desarrollar aplicaciones escalables y de alto rendimiento. Elixir se basa en la Máquina Virtual de Erlang (BEAM), lo que le proporciona una serie de características, como la concurrencia ligera, la mensajería asíncrona y la tolerancia a fallos.