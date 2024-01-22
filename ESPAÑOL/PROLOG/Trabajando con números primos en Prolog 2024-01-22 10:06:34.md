```prolog
% Definimos un predicado `es_primo` que comprueba si un número es primo.
es_primo(N) :-
  N > 1,
  \+ (N mod 2 = 0),
  \+ (N mod 3 = 0),
  \+ (N mod 5 = 0),
  \+ (N mod 7 = 0),
  \+ (N mod 11 = 0),
  \+ (N mod 13 = 0),
  \+ (N mod 17 = 0),
  \+ (N mod 19 = 0),
  \+ (N mod 23 = 0),
  \+ (N mod 29 = 0),
  \+ (N mod 31 = 0),
  \+ (N mod 37 = 0),
  \+ (N mod 41 = 0),
  \+ (N mod 43 = 0),
  \+ (N mod 47 = 0),
  \+ (N mod 53 = 0),
  \+ (N mod 59 = 0),
  \+ (N mod 61 = 0),
  \+ (N mod 67 = 0),
  \+ (N mod 71 = 0),
  \+ (N mod 73 = 0),
  \+ (N mod 79 = 0),
  \+ (N mod 83 = 0),
  \+ (N mod 89 = 0),
  \+ (N mod 97 = 0).

% Definimos un predicado `generar_primos` que genera una lista de números primos.
generar_primos(L) :-
  findall(N, (between(2, 1000000), es_primo(N)), L).

% Definimos un predicado `suma_primos` que calcula la suma de los números primos de una lista.
suma_primos(L, S) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  sumlist(Ns, S).

% Definimos un predicado `producto_primos` que calcula el producto de los números primos de una lista.
producto_primos(L, P) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  foldl((\N1, \N2, \P) -> \N1 * \N2, 1, Ns, P).

% Definimos un predicado `maximo_primo` que calcula el máximo número primo de una lista.
maximo_primo(L, M) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  max_list(Ns, M).

% Definimos un predicado `minimo_primo` que calcula el mínimo número primo de una lista.
minimo_primo(L, M) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  min_list(Ns, M).

% Definimos un predicado `media_primos` que calcula la media de los números primos de una lista.
media_primos(L, M) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  sumlist(Ns, S),
  length(Ns, N),
  M is S / N.

% Definimos un predicado `desviacion_estandar_primos` que calcula la desviación estándar de los números primos de una lista.
desviacion_estandar_primos(L, DS) :-
  findall(N, (member(N, L), es_primo(N)), Ns),
  mean(Ns, M),
  findall(D, (member(N, Ns), D is N - M), Ds),
  square_list(Ds, Ds2),
  sumlist(Ds2, S),
  length(Ns, N),
  DS is sqrt(S / N).
```

Este código define una serie de predicados para trabajar con números primos:

* `es_primo(N)`: Comprueba si un número es primo.
* `generar_primos(L)`: Genera una lista de números primos.
* `suma_primos(L, S)`: Calcula la suma de los números primos de una lista.
* `producto_primos(L, P)`: Calcula el producto de los números primos de una lista.
* `maximo_primo(L, M)`: Calcula el máximo número primo de una lista.
* `minimo_primo(L, M)`: Calcula el mínimo número primo de una lista.
* `media_primos(L, M)`: Calcula la media de los números primos de una lista.
* `desviacion_estandar_primos(L, DS)`: Calcula la desviación estándar de los números primos de una lista.