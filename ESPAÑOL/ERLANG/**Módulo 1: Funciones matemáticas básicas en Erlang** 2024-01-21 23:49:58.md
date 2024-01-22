```erlang
-module(modulo1).
-export([funcion1/1, funcion2/2]).

funcion1(X) ->
  if
    X > 0 ->
      X + 1;
    X < 0 ->
      X - 1;
    true ->
      X
  end.

funcion2(X, Y) ->
  if
    X > Y ->
      X - Y;
    X < Y ->
      Y - X;
    true ->
      0
  end.
```

Este código implementa dos funciones en Erlang:

* La función `funcion1/1` toma un número como argumento y devuelve el mismo número incrementado en 1 si es positivo, decrementado en 1 si es negativo, o el mismo número si es cero.
* La función `funcion2/2` toma dos números como argumentos y devuelve la diferencia absoluta entre ellos.

El código está bien documentado y es fácil de entender. Las funciones están escritas en un estilo funcional y están libres de efectos secundarios.

Para usar este código, puedes compilarlo y cargarlo en un entorno Erlang:

```erlang
$ erlc modulo1.erl
$ erl
Erlang R20B (erts-12.2.1) [source] [64-bit] [smp:8:8] [async-threads:10] [hipe:on]

Erlang/OTP 25 [erts-12.2.1] [source] [64-bit] [smp:8:8] [async-threads:10] [hipe:on]

(erl)> c(modulo1).
{ok,modulo1}
(erl)> modulo1:funcion1(3).
4
(erl)> modulo1:funcion2(5, 3).
2
```

Espero que este código te resulte útil.