```erlang
-module(big_code).
-export([main/0]).

main() ->
    io:format("Esto es un código complejo en Erlang~n"),
    start_processes(),
    wait_for_processes(),
    combine_results(),
    display_results(),
    ok.

start_processes() ->
    lists:foreach(
        fun(N) ->
            spawn_link(fun() -> process(N) end)
        end,
        lists:seq(1, 10)
    ).

process(N) ->
    Result = N * 2,
    io:format("Proceso ~B: Resultado = ~B~n", [N, Result]),
    Result.

wait_for_processes() ->
    receive
        Result -> io:format("Resultado recibido: ~B~n", [Result])
    after 1000 -> io:format("Esperando resultados...~n")
    end,
    wait_for_processes().

combine_results() ->
    lists:foldl(
        fun(Result, Acc) -> Acc + Result end,
        0,
        lists:seq(1, 10)
    ).

display_results() ->
    io:format("Resultados combinados: ~B~n", [combine_results()]).
```

Explicación:

1. **Módulo principal:**

    ```erlang
    -module(big_code).
    ```

    Esto define el módulo principal, que es el punto de entrada del programa.

2. **Función principal:**

    ```erlang
    -export([main/0]).
    ```

    Esta línea exporta la función `main/0` para que pueda ser llamada desde fuera del módulo.

3. **Definición de la función principal:**

    ```erlang
    main() ->
        io:format("Esto es un código complejo en Erlang~n"),
        start_processes(),
        wait_for_processes(),
        combine_results(),
        display_results(),
        ok.
    ```

    Esta es la definición de la función principal, que se ejecuta cuando se llama al módulo.

    1. `io:format("Esto es un código complejo en Erlang~n")`: Muestra un mensaje en la consola.
    2. `start_processes()`: Inicia un proceso para cada número del 1 al 10.
    3. `wait_for_processes()`: Espera a que todos los procesos terminen.
    4. `combine_results()`: Combina los resultados de todos los procesos.
    5. `display_results()`: Muestra los resultados combinados en la consola.
    6. `ok`: Devuelve el valor `ok` para indicar que el programa se ejecutó correctamente.

4. **Función `start_processes()`:**

    ```erlang
    start_processes() ->
        lists:foreach(
            fun(N) ->
                spawn_link(fun() -> process(N) end)
            end,
            lists:seq(1, 10)
        ).
    ```

    Esta función inicia un proceso para cada número del 1 al 10.

    1. `lists:foreach`: Es una función que itera sobre una lista y ejecuta una función para cada elemento de la lista.
    2. `spawn_link(fun() -> process(N) end)`: Inicia un nuevo proceso (un hilo en Erlang) que ejecuta la función `process/1` con el argumento `N`.
    3. `lists:seq(1, 10)`: Genera una lista de números del 1 al 10.

5. **Función `process/1`:**

    ```erlang
    process(N) ->
        Result = N * 2,
        io:format("Proceso ~B: Resultado = ~B~n", [N, Result]),
        Result.
    ```

    Esta función es ejecutada por el proceso iniciado en `start_processes()`.

    1. `Result = N * 2`: Calcula el resultado de multiplicar `N` por 2.
    2. `io:format("Proceso ~B: Resultado = ~B~n", [N, Result])`: Muestra un mensaje en la consola que incluye el número de proceso y el resultado.
    3. `Result`: Devuelve el resultado del cálculo.

6. **Función `wait_for_processes()`:**

    ```erlang
    wait_for_processes() ->
        receive
            Result -> io:format("Resultado recibido: ~B~n", [Result])
        after 1000 -> io:format("Esperando resultados...~n")
        end,
        wait_for_processes().
    ```

    Esta función espera a que todos los procesos terminen.

    1. `receive`: Es una instrucción que espera a que se reciba un mensaje.
    2. `Result -> io:format("Resultado recibido: ~B~n", [Result])`: Si se recibe un mensaje con un resultado, lo muestra en la consola.
    3. `after 1000 -> io:format("Esperando resultados...~n")`: Si no se recibe un mensaje en los próximos 1000 milisegundos, muestra un mensaje en la consola.
    4. `wait_for_processes()`: Se llama a sí misma recursivamente hasta que todos los procesos hayan terminado.

7. **Función `combine_results()`:**

    ```erlang
    combine_results() ->
        lists:foldl(
            fun(Result, Acc) -> Acc + Result end,
            0,
            lists:seq(1, 10)
        ).
    ```

    Esta función combina los resultados de todos los procesos.

    1. `lists:foldl`: Es una función que reduce una lista a un solo valor aplicando una función a cada elemento de la lista.
    2. `fun(Result, Acc) -> Acc + Result end`: Es una función que suma el resultado actual con el acumulador.
    3. `0`: Es el valor inicial del acumulador.
    4. `lists:seq(1, 10)`: Genera una lista de números del 1 al 10.

8. **Función `display_results()`:**

    ```erlang
    display_results() ->
        io:format("Resultados combinados: ~B~n", [combine_results()]).
    ```

    Esta función muestra los resultados combinados en la consola.

    1. `io:format("Resultados combinados: ~B~n", [combine_results()])`: Muestra un mensaje en la consola que incluye los resultados combinados.