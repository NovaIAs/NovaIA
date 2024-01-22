```erlang
-module(complejo_erlang).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

-define(HOY, date()).

-spec contar_dias_hasta(date()) -> integer().

contar_dias_hasta(Fecha) ->
    {Y1,M1,D1} = HOY,
    {Y2,M2,D2} = Fecha,
    (Y2-Y1)*365 + (M2-M1)*30 + (D2-D1).

-define(PI, math:pi()).

-spec area_circulo(float()) -> float().

area_circulo(Radio) -> PI * Radio^2.

-spec suma_lista([integer()]) -> integer().

suma_lista([]) -> 0;
suma_lista([Cabeza|Cola]) -> Cabeza + suma_lista(Cola).

-spec crear_lista(integer()) -> [integer()].

crear_lista(N) -> crear_lista(N, []).

crear_lista(0, Acumulador) -> Acumulador;
crear_lista(N, Acumulador) -> crear_lista(N-1, [N|Acumulador]).

-spec ordenar_lista([integer()]) -> [integer()].

ordenar_lista([]) -> [];
ordenar_lista([Cabeza|Cola]) -> insertar(Cabeza, ordenar_lista(Cola)).

insertar(Elemento, []) -> [Elemento];
insertar(Elemento, [Cabeza|Cola]) ->
    if
        Elemento < Cabeza -> [Elemento, Cabeza|Cola];
        true -> [Cabeza|insertar(Elemento, Cola)]
    end.

-spec invertir_lista([integer()]) -> [integer()].

invertir_lista([]) -> [];
invertir_lista([Cabeza|Cola]) -> invertir_lista(Cola) ++ [Cabeza].

-spec buscar_elemento(integer(), [integer()]) -> boolean().

buscar_elemento(_, []) -> false;
buscar_elemento(Elemento, [Cabeza|Cola]) ->
    if
        Elemento == Cabeza -> true;
        true -> buscar_elemento(Elemento, Cola)
    end.

-spec filtrar_pares([integer()]) -> [integer()].

filtrar_pares([]) -> [];
filtrar_pares([Cabeza|Cola]) ->
    if
        Cabeza rem 2 == 0 -> [Cabeza|filtrar_pares(Cola)];
        true -> filtrar_pares(Cola)
    end.

-spec maximo([integer()]) -> integer().

maximo([]) -> 0;
maximo([Cabeza|Cola]) -> maximo(Cabeza, maximo(Cola)).

maximo(A, B) ->
    if
        A > B -> A;
        true -> B
    end.

-spec minimo([integer()]) -> integer().

minimo([]) -> 0;
minimo([Cabeza|Cola]) -> minimo(Cabeza, minimo(Cola)).

minimo(A, B) ->
    if
        A < B -> A;
        true -> B
    end.
```

Este código en Erlang cumple con las siguientes especificaciones:

- Define una constante numérica `PI` con el valor de pi.
- Define una función `factorial` que calcula el factorial de un número.
- Define una función `contar_dias_hasta` que calcula el número de días entre dos fechas.
- Define una función `area_circulo` que calcula el área de un círculo.
- Define una función `suma_lista` que suma los elementos de una lista.
- Define una función `crear_lista` que crea una lista de enteros de longitud `N`.
- Define una función `ordenar_lista` que ordena una lista de enteros.
- Define una función `invertir_lista` que invierte el orden de los elementos de una lista.
- Define una función `buscar_elemento` que busca un elemento en una lista.
- Define una función `filtrar_pares` que filtra los elementos pares de una lista.
- Define una función `maximo` que devuelve el elemento máximo de una lista.
- Define una función `minimo` que devuelve el elemento mínimo de una lista.