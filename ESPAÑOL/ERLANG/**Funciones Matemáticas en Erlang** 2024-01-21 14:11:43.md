```erlang
-module(complejo).
-export([fibonacci/1, factorial/1, primo/1, suma_pares/1, invertir_lista/1]).

% Calcula el n-ésimo número de Fibonacci
fibonacci(N) when N =< 1 -> 1;
fibonacci(N) -> fibonacci(N - 1) + fibonacci(N - 2).

% Calcula el factorial de un número
factorial(0) -> 1;
factorial(N) -> N * factorial(N - 1).

% Determina si un número es primo
primo(N) when N < 2 -> false;
primo(2) -> true;
primo(N) -> es_primo(N, 2).

es_primo(N, I) when I * I > N -> true;
es_primo(N, I) when N rem I =:= 0 -> false;
es_primo(N, I) -> es_primo(N, I + 1).

% Suma los números pares de una lista
suma_pares([]) -> 0;
suma_pares([H | T]) ->
  if
    H rem 2 =:= 0 -> H + suma_pares(T);
    true -> suma_pares(T)
  end.

% Invierte una lista
invertir_lista([]) -> [];
invertir_lista([H | T]) -> invertir_lista(T) ++ [H].
```

Explicación del código:

* La función `fibonacci/1` calcula el n-ésimo número de Fibonacci. Si `N` es menor o igual que 1, devuelve 1. De lo contrario, devuelve la suma del n-ésimo y el (n-1)-ésimo número de Fibonacci.

* La función `factorial/1` calcula el factorial de un número. Si `N` es 0, devuelve 1. De lo contrario, devuelve `N` multiplicado por el factorial de `N-1`.

* La función `primo/1` determina si un número es primo. Si `N` es menor que 2, devuelve `false`. Si `N` es 2, devuelve `true`. De lo contrario, llama a la función auxiliar `es_primo/2`, que devuelve `true` si `N` es primo y `false` en caso contrario.

* La función `suma_pares/1` suma los números pares de una lista. Si la lista está vacía, devuelve 0. De lo contrario, comprueba el primer elemento de la lista. Si es par, lo suma al resultado de la suma de los números pares del resto de la lista. Si es impar, simplemente llama a la función recursivamente con el resto de la lista.

* La función `invertir_lista/1` invierte una lista. Si la lista está vacía, devuelve una lista vacía. De lo contrario, llama a la función recursivamente con el resto de la lista y añade el primer elemento de la lista al final de la lista invertida.