```elixir
# Módulo para gestionar las operaciones con fechas
defmodule DateUtils do
  # Función para obtener la fecha actual
  def get_current_date() do
    NaiveDateTime.local_now()
  end

  # Función para obtener la diferencia entre dos fechas
  def date_difference(date1, date2) do
    date1.difference(date2)
  end

  # Función para sumar o restar una cantidad de días a una fecha
  def add_days(date, days) do
    date.add(days * 24 * 60 * 60)
  end
end

# Módulo para gestionar las operaciones con cadenas
defmodule StringUtils do
  # Función para convertir una cadena a mayúsculas
  def to_uppercase(string) do
    String.upcase(string)
  end

  # Función para convertir una cadena a minúsculas
  def to_lowercase(string) do
    String.downcase(string)
  end

  # Función para dividir una cadena en una lista de cadenas
  def split(string, delimiter) do
    String.split(string, delimiter)
  end
end

# Módulo para gestionar las operaciones con listas
defmodule ListUtils do
  # Función para obtener el primer elemento de una lista
  def get_first(list) do
    list |> Enum.at(0)
  end

  # Función para obtener el último elemento de una lista
  def get_last(list) do
    list |> Enum.at(-1)
  end

  # Función para eliminar un elemento de una lista
  def remove(list, element) do
    list |> Enum.reject(&(&1 == element))
  end
end

# Módulo para gestionar la entrada y salida de datos
defmodule IOUtils do
  # Función para leer una línea de texto de la consola
  def read_line() do
    IO.gets("") |> String.trim()
  end

  # Función para imprimir un mensaje en la consola
  def print(message) do
    IO.puts(message)
  end
end

# Programa principal
defmodule Main do
  # Función principal
  def main() do
    # Obtenemos la fecha actual
    date = DateUtils.get_current_date()

    # Sumamos 10 días a la fecha actual
    new_date = DateUtils.add_days(date, 10)

    # Convertimos la fecha a una cadena
    date_string = NaiveDateTime.to_string(date)

    # Dividimos la cadena de la fecha en una lista de cadenas
    date_parts = StringUtils.split(date_string, "-")

    # Obtenemos el año, el mes y el día de la fecha
    year = ListUtils.get_first(date_parts)
    month = ListUtils.get(date_parts, 1)
    day = ListUtils.get_last(date_parts)

    # Imprimimos la fecha actual, la nueva fecha y sus componentes
    IOUtils.print("Fecha actual: #{date_string}")
    IOUtils.print("Nueva fecha: #{NaiveDateTime.to_string(new_date)}")
    IOUtils.print("Año: #{year}")
    IOUtils.print("Mes: #{month}")
    IOUtils.print("Día: #{day}")
  end
end

# Ejecutamos el programa principal
Main.main()
```

Explicación del código:

* El módulo `DateUtils` proporciona funciones para trabajar con fechas. La función `get_current_date()` devuelve la fecha actual, `date_difference()` calcula la diferencia entre dos fechas y `add_days()` suma o resta una cantidad de días a una fecha.
* El módulo `StringUtils` proporciona funciones para trabajar con cadenas. La función `to_uppercase()` convierte una cadena a mayúsculas, `to_lowercase()` convierte una cadena a minúsculas y `split()` divide una cadena en una lista de cadenas.
* El módulo `ListUtils` proporciona funciones para trabajar con listas. La función `get_first()` devuelve el primer elemento de una lista, `get_last()` devuelve el último elemento de una lista y `remove()` elimina un elemento de una lista.
* El módulo `IOUtils` proporciona funciones para la entrada y salida de datos. La función `read_line()` lee una línea de texto de la consola e `print()` imprime un mensaje en la consola.
* El módulo `Main` contiene la función principal del programa. La función `main()` obtiene la fecha actual, suma 10 días a la fecha actual y convierte la fecha a una cadena. Luego divide la cadena de la fecha en una lista de cadenas y obtiene el año, el mes y el día de la fecha. Finalmente, imprime la fecha actual, la nueva fecha y sus componentes.