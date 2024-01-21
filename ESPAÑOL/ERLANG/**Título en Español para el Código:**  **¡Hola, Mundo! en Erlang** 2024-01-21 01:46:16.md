```erlang
:-module(hola_mundo).
:-export([saluda/0]).

saluda() ->
  io:format("¡Hola, mundo!").

```

Este código es una sencilla aplicación Erlang que imprime el mensaje "¡Hola, mundo!" en la consola.

La primera línea del código define el módulo de la aplicación, que en este caso es `hola_mundo`.

La segunda línea define las funciones exportadas por el módulo, que en este caso es la función `saluda/0`.

La función `saluda/0` imprime el mensaje "¡Hola, mundo!" en la consola usando la función `io:format/1`.

Para ejecutar la aplicación, tienes que compilarla con el comando `erlc`.

```
erlc hola_mundo.erl
```

Esto creará un archivo `hola_mundo.beam` que contiene el código compilado de la aplicación.

Para ejecutar la aplicación compilada, tienes que usar el comando `erl`.

```
erl hola_mundo
```

Esto iniciará la aplicación Erlang y ejecutará la función `saluda/0`.

El mensaje "¡Hola, mundo!" se imprimirá en la consola.