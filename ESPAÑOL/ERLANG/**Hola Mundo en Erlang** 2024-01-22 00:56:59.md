```erlang
-module(hola_mundo).
-export([main/0]).

main() ->
    io:fwrite("¡Hola, mundo!").
```

Este código crea un módulo llamado `hola_mundo` con una función de exportación llamada `main`. La función `main` simplemente imprime el mensaje "¡Hola, mundo!" en la consola.

Para ejecutar este código, sigue estos pasos:

1. Abre un editor de texto y pega el código anterior. Guárdalo en un archivo llamado `hola_mundo.erl`.
2. Abre una terminal y navega hasta el directorio donde guardaste el archivo `.erl`.
3. Escribe el siguiente comando para compilar el código:

```
erlc hola_mundo.erl
```

Esto creará un archivo llamado `hola_mundo.beam`.

4. Escribe el siguiente comando para ejecutar el código:

```
erlang -noshell -s hola_mundo main
```

Esto ejecutará la función `main` en el módulo `hola_mundo`.

Ahora deberías ver el mensaje "¡Hola, mundo!" impreso en la consola.

Este es un código muy simple, pero puede usarse como base para crear programas Erlang más complejos.

Aquí tienes algunos ejemplos de código Erlang más complejo:

* [Un servidor web simple](https://github.com/erlang/otp/blob/master/lib/webmachine/examples/simple_server.erl)
* [Un cliente de chat](https://github.com/erlang/otp/blob/master/lib/inet/examples/chat/client.erl)
* [Un servidor de chat](https://github.com/erlang/otp/blob/master/lib/inet/examples/chat/server.erl)

Estos son solo algunos ejemplos, y hay muchos otros programas Erlang complejos que puedes encontrar en línea.