```elixir
# Creación de un módulo personalizado
defmodule MiModulo do
  # Definición de una función
  def funcion_principal() do
    # Impresión de un mensaje en la consola
    IO.puts("¡Hola, mundo!")

    # Creación de un número aleatorio entre 1 y 100
    numero_aleatorio = :rand.uniform(100)

    # Impresión del número aleatorio
    IO.puts("El número aleatorio es: #{numero_aleatorio}")

    # Creación de una lista de números del 1 al 10
    lista_numeros = Enum.range(1, 10)

    # Impresión de la lista de números
    IO.puts("La lista de números es: #{lista_numeros}")

    # Creación de un mapa con pares clave-valor
    mapa_datos = %{nombre: "Juan", edad: 30, ciudad: "Madrid"}

    # Impresión del mapa de datos
    IO.puts("El mapa de datos es: #{mapa_datos}")

    # Creación de una tupla con elementos heterogéneos
    tupla_datos = {1, "Hola", true}

    # Impresión de la tupla de datos
    IO.puts("La tupla de datos es: #{tupla_datos}")

    # Creación de una función anónima
    funcion_anonima = fn x -> x * 2 end

    # Invocación de la función anónima con un argumento
    resultado = funcion_anonima.(5)

    # Impresión del resultado
    IO.puts("El resultado de la función anónima es: #{resultado}")

    # Creación de un proceso (actor) utilizando una función anónima
    pid = spawn(fn ->
      # El proceso envía un mensaje a la consola
      IO.puts("¡Hola desde el proceso!")

      # El proceso se detiene a sí mismo
      Process.exit(self())
    end)

    # Espera a que el proceso termine su ejecución
    Process.monitor(pid)
    receive do
      {:DOWN, _ref, :process, _pid, _reason} ->
        # El proceso ha terminado su ejecución
        IO.puts("El proceso ha terminado")
    end
  end
end

# Invocación de la función principal del módulo
MiModulo.funcion_principal()
```

**Explicación del código:**

1. **Creación de un módulo personalizado:** Se crea un módulo personalizado llamado `MiModulo` utilizando la palabra clave `defmodule`.

2. **Definición de una función:** Dentro del módulo, se define una función llamada `funcion_principal` utilizando la palabra clave `def`.

3. **Impresión de un mensaje en la consola:** Se utiliza la función `IO.puts` para imprimir el mensaje "Hola, mundo!" en la consola.

4. **Creación de un número aleatorio:** Se genera un número aleatorio entre 1 y 100 utilizando la función `:rand.uniform(100)`.

5. **Impresión del número aleatorio:** Se imprime el número aleatorio generado utilizando la función `IO.puts`.

6. **Creación de una lista de números:** Se crea una lista de números del 1 al 10 utilizando la función `Enum.range(1, 10)`.

7. **Impresión de la lista de números:** Se imprime la lista de números creada utilizando la función `IO.puts`.

8. **Creación de un mapa con pares clave-valor:** Se crea un mapa con pares clave-valor utilizando la sintaxis `{clave: valor}`. En este caso, se crea un mapa con las claves "nombre", "edad" y "ciudad" y los valores correspondientes.

9. **Impresión del mapa de datos:** Se imprime el mapa de datos creado utilizando la función `IO.puts`.

10. **Creación de una tupla con elementos heterogéneos:** Se crea una tupla con elementos heterogéneos utilizando la sintaxis `(elemento1, elemento2, elemento3)`. En este caso, se crea una tupla con los elementos 1, "Hola" y true.

11. **Impresión de la tupla de datos:** Se imprime la tupla de datos creada utilizando la función `IO.puts`.

12. **Creación de una función anónima:** Se crea una función anónima utilizando la sintaxis `fn x -> x * 2 end`. 
Esta función toma un argumento `x` y devuelve el doble de su valor.

13. **Invocación de la función anónima con un argumento:** Se invoca la función anónima creada con el valor 5 como argumento. El resultado de la invocación se almacena en la variable `resultado`.

14. **Impresión del resultado:** Se imprime el resultado de la invocación de la función anónima utilizando la función `IO.puts`.

15. **Creación de un proceso (actor) utilizando una función anónima:** Se crea un proceso utilizando la función `spawn` y una función anónima. 
La función anónima envía un mensaje a la consola y luego se detiene a sí misma. 

16. **Espera a que el proceso termine su ejecución:** Se utiliza la función `Process.monitor` para esperar a que el proceso termine su ejecución. 
Cuando el proceso termina, se recibe un mensaje de notificación en el buzón de correo del proceso principal.

17. **Impresión de un mensaje cuando el proceso termina:** Cuando se recibe el mensaje de notificación, se imprime un mensaje en la consola indicando que el proceso ha terminado.

Este código crea un módulo personalizado con una función principal que realiza diversas operaciones, incluyendo la impresión de mensajes, la generación de números aleatorios, la creación de listas, mapas y tuplas, la invocación de funciones anónimas y la creación de procesos. Es un ejemplo complejo y completo de código Elixir que demuestra las diversas capacidades del lenguaje.