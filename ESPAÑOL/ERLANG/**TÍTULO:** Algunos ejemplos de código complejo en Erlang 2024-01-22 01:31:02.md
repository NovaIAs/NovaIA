```erlang
% Crear una función que reciba una lista de números y devuelva la suma de todos los números de la lista.

-module(suma_lista).
-export([suma/1]).

suma([]) ->
    0;
suma([Cabeza | Cola]) ->
    Cabeza + suma(Cola).

% Crear una función que reciba una lista de cadenas y devuelva una lista con todas las cadenas en mayúsculas.

-module(mayusculas).
-export([mayus/1]).

mayus([]) ->
    [];
mayus([Cabeza | Cola]) ->
    [string:to_upper(Cabeza) | mayus(Cola)].

% Crear una función que reciba un número y devuelva una lista con todos los números primos menores que el número recibido.

-module(primos).
-export([primos/1]).

primos(N) ->
    primos(N, 2, []).

primos(N, I, Primos) when I > N ->
    Primos;
primos(N, I, Primos) when N rem I =:= 0 ->
    primos(N, I + 1, Primos);
primos(N, I, Primos) ->
    primos(N, I + 1, [I | Primos]).

% Crear una función que reciba una lista de cadenas y devuelva un mapa con las cadenas como claves y su longitud como valores.

-module(longitud_cadena).
-export([longitud/1]).

longitud([]) ->
    #{};
longitud([Cabeza | Cola]) ->
    Mapa = longitud(Cola),
    maps:put(Cabeza, string:len(Cabeza), Mapa).

% Crear una función que reciba una lista de números y devuelva un mapa con los números como claves y su factoriales como valores.

-module(factorial).
-export([factorial/1]).

factorial([]) ->
    #{};
factorial([Cabeza | Cola]) ->
    Mapa = factorial(Cola),
    maps:put(Cabeza, factorial(Cabeza), Mapa).

factorial(N) when N =< 1 ->
    1;
factorial(N) ->
    N * factorial(N - 1).
```

Estos son cinco ejemplos de código complejo en Erlang. Cada función tiene su propia explicación para que puedas entender lo que hace.