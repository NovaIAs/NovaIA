**Código en ERLANG:**

```erlang
-module(proveedor_de_nombres).
-export([iniciar/0, detener/0, solicitar_nombre/0]).

iniciar() ->
    register(proveedor_de_nombres, self()),
    loop().

detener() ->
    proveedor_de_nombres ! detener.

solicitar_nombre() ->
    proveedor_de_nombres ! {nombre, self()},
    receive
        {nombre, Nombre} -> Nombre
    end.

loop() ->
    receive
        detener -> ok;
        {nombre, Pid} ->
            Pid ! {nombre, generar_nombre()},
            loop()
    end.

generar_nombre() ->
    N = math:random(999),
    list_to_atom("nombre_" ++ integer_to_list(N)).
```

**Explicación del código:**

* El módulo `proveedor_de_nombres` exporta las siguientes funciones:
    * `iniciar/0`: Inicia el servidor de nombres.
    * `detener/0`: Detiene el servidor de nombres.
    * `solicitar_nombre/0`: Solicita un nombre aleatorio al servidor de nombres.

* La función `iniciar/0` registra el servidor de nombres con el átomo `proveedor_de_nombres` y llama a la función `loop/0` para iniciar el bucle principal del servidor.

* La función `detener/0` envía un mensaje `detener` al servidor de nombres para que se detenga.

* La función `solicitar_nombre/0` envía un mensaje `{nombre, self()}` al servidor de nombres y espera a recibir una respuesta con el nombre generado.

* La función `loop/0` es el bucle principal del servidor de nombres. Espera a recibir mensajes `{nombre, Pid}` y genera un nombre aleatorio para enviar a `Pid`.

* La función `generar_nombre/0` genera un nombre aleatorio usando la función `math:random/1` y el átomo `"nombre_"`.

Este código crea un servidor de nombres que puede ser utilizado por otros procesos para solicitar nombres aleatorios. El servidor de nombres se puede iniciar y detener utilizando las funciones `iniciar/0` y `detener/0`, respectivamente. Los procesos pueden solicitar un nombre aleatorio al servidor de nombres utilizando la función `solicitar_nombre/0`.