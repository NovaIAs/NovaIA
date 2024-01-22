```erlang
-module(compruébalo_y_cuenta).
-export([compruébalo_y_cuenta/2]).

compruébalo_y_cuenta(Lista, Objetivo) ->
    compruébalo_y_cuenta(Lista, Objetivo, 0).

compruébalo_y_cuenta([], _, Cuenta) ->
    Cuenta;
compruébalo_y_cuenta([Cabeza | Resto], Objetivo, Cuenta) ->
    if
        Cabeza =< Objetivo ->
            compruébalo_y_cuenta(Resto, Objetivo, Cuenta + 1);
        true ->
            Cuenta
    end.
```

Explicación:

* El módulo `compruébalo_y_cuenta` define una función llamada `compruébalo_y_cuenta/2` que toma dos argumentos: una lista y un objetivo.
* La función `compruébalo_y_cuenta/2` devuelve el número de elementos de la lista que son menores o iguales que el objetivo.
* La función `compruébalo_y_cuenta/3` es una función recursiva que toma tres argumentos: una lista, un objetivo y una cuenta.
* La función `compruébalo_y_cuenta/3` devuelve el número de elementos de la lista que son menores o iguales que el objetivo, más uno si el primer elemento de la lista es menor o igual que el objetivo.
* La función `compruébalo_y_cuenta/2` llama a la función `compruébalo_y_cuenta/3` con el tercer argumento establecido en 0.
* La función `compruébalo_y_cuenta/3` llama a sí misma recursivamente con el segundo elemento de la lista, el objetivo y la cuenta incrementada en uno si el primer elemento de la lista es menor o igual que el objetivo.
* La función `compruébalo_y_cuenta/3` devuelve la cuenta si la lista está vacía o si el primer elemento de la lista es mayor que el objetivo.