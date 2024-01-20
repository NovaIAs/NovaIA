```erlang
-module(complejo).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

-module(tabla_multiplicar).
-export([tabla/1]).

tabla(N) ->
    io:format("Tabla de multiplicar de ~p:~n", [N]),
    [io:format("~p x ~p = ~p~n", [N, I, N*I]) || I <- lists:seq(1, 10)].

-module(ordenar_lista).
-export([ordenar/1]).

ordenar([]) -> [];
ordenar([H|T]) ->
    Ins = ordenar(T),
    ins_sort(H, Ins).

ins_sort(X, []) -> [X];
ins_sort(X, [H|T]) when X =< H -> [X|H|T];
ins_sort(X, [H|T]) -> [H|ins_sort(X, T)].

-module(fibonacci).
-export([fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(N) when N > 1 -> fib(N-1) + fib(N-2).

-module(mayor_comun_divisor).
-export([mcd/2]).

mcd(A, 0) -> A;
mcd(A, B) -> mcd(B, A rem B).

-module(menor_comun_multiplo).
-export([mcm/2]).

mcm(A, B) -> A * B div mcd(A, B).

-module(primo).
-export([es_primo/1]).

es_primo(1) -> false;
es_primo(N) ->
    not lists:any(fun(I) -> N rem I =:= 0 end, lists:seq(2, trunc(math:sqrt(N)))).

-module(convertir_entero_a_romano).
-export([entero_a_romano/1]).

entero_a_romano(0) -> "";
entero_a_romano(N) ->
    Miles = N div 1000,
    Centenas = (N rem 1000) div 100,
    Decenas = (N rem 100) div 10,
    Unidades = N rem 10,
    Miles_romano = miles_a_romano(Miles),
    Centenas_romano = centenas_a_romano(Centenas),
    Decenas_romano = decenas_a_romano(Decenas),
    Unidades_romano = unidades_a_romano(Unidades),
    Miles_romano ++ Centenas_romano ++ Decenas_romano ++ Unidades_romano.

miles_a_romano(0) -> "";
miles_a_romano(1) -> "M";
miles_a_romano(2) -> "MM";
miles_a_romano(3) -> "MMM".

centenas_a_romano(0) -> "";
centenas_a_romano(1) -> "C";
centenas_a_romano(2) -> "CC";
centenas_a_romano(3) -> "CCC";
centenas_a_romano(4) -> "CD";
centenas_a_romano(5) -> "D";
centenas_a_romano(6) -> "DC";
centenas_a_romano(7) -> "DCC";
centenas_a_romano(8) -> "DCCC";
centenas_a_romano(9) -> "CM".

decenas_a_romano(0) -> "";
decenas_a_romano(1) -> "X";
decenas_a_romano(2) -> "XX";
decenas_a_romano(3) -> "XXX";
decenas_a_romano(4) -> "XL";
decenas_a_romano(5) -> "L";
decenas_a_romano(6) -> "LX";
decenas_a_romano(7) -> "LXX";
decenas_a_romano(8) -> "LXXX";
decenas_a_romano(9) -> "XC".

unidades_a_romano(0) -> "";
unidades_a_romano(1) -> "I";
unidades_a_romano(2) -> "II";
unidades_a_romano(3) -> "III";
unidades_a_romano(4) -> "IV";
unidades_a_romano(5) -> "V";
unidades_a_romano(6) -> "VI";
unidades_a_romano(7) -> "VII";
unidades_a_romano(8) -> "VIII";
unidades_a_romano(9) -> "IX".
```

Explicación del código:

* **factorial:** Calcula el factorial de un número.
* **tabla:** Genera la tabla de multiplicar de un número.
* **ordenar:** Ordena una lista de números en orden ascendente.
* **fibonacci:** Calcula el número de Fibonacci de un número.
* **mcd:** Calcula el máximo común divisor de dos números.
* **mcm:** Calcula el mínimo común múltiplo de dos números.
* **primo:** Comprueba si un número es primo.
* **entero_a_romano:** Convierte un número entero a su equivalente en números romanos.
* **miles_a_romano:** Convierte un número de miles a su equivalente en números romanos.
* **centenas_a_romano:** Convierte un número de centenas a su equivalente en números romanos.
* **decenas_a_romano:** Convierte un número de decenas a su equivalente en números romanos.
* **unidades_a_romano:** Convierte un número de unidades a su equivalente en números romanos.