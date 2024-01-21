```erlang
-module(hola_mundo).
-export([hola/0]).

hola() ->
    io:fwrite("¡Hola, mundo!").
```

Este es un programa simple en Erlang que imprime "Hola, mundo!" a la consola. El módulo `hola_mundo` define una función `hola` exportada, que imprime el mensaje a la consola.

```erlang
-module(contador).
-export([inc/1, get/0]).

-record(contador, {valor = 0}).

inc(Contador) ->
    Contador#contador{valor = Contador#contador.valor + 1}.

get(Contador) ->
    Contador#contador.valor.
```

Este programa define un módulo `contador` que encapsula un contador. El contador se representa como un registro con un campo `valor` que se inicializa a 0. El módulo exporta dos funciones: `inc`, que incrementa el contador, y `get`, que devuelve el valor actual del contador.

```erlang
-module(factorial).
-export([fact/1]).

fact(0) ->
    1;
fact(N) when N > 0 ->
    N * fact(N-1).
```

Este programa define un módulo `factorial` que calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales a ese número. El módulo exporta una función `fact` que calcula el factorial de un número.

```erlang
-module(lista).
-export([max/1]).

max([H | T]) ->
    max(H, max(T));
max(A, B) when A > B ->
    A;
max(_, B) ->
    B.
```

Este programa define un módulo `lista` que contiene una función `max` que devuelve el elemento máximo de una lista. La función `max` se define de forma recursiva: si la lista tiene un solo elemento, devuelve ese elemento. De lo contrario, llama a `max` en la cabeza de la lista y en el resto de la lista, y devuelve el mayor de los dos valores.

```erlang
-module(ordenar).
-export([sort/1]).

sort([]) ->
    [];
sort([H | T]) ->
    insert(H, sort(T)).

insert(X, []) ->
    [X];
insert(X, [H | T]) when X =< H ->
    [X | [H | T]];
insert(X, [H | T]) ->
    [H | insert(X, T)].
```

Este programa define un módulo `ordenar` que contiene una función `sort` que ordena una lista de números en orden ascendente. La función `sort` se define de forma recursiva: si la lista está vacía, devuelve una lista vacía. De lo contrario, llama a `insert` en la cabeza de la lista y en el resto de la lista, y devuelve la lista resultante. La función `insert` inserta un elemento en una lista ordenada de forma que la lista siga ordenada.