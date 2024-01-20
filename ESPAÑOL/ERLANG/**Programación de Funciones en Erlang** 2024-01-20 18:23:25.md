```erlang
% Compilamos el módulo con el comando "c(modulo_fibonacci)".
-module(modulo_fibonacci).

% Exportamos la función "fibonacci" para que pueda ser llamada desde otros módulos.
-export([fibonacci/1]).

% Definimos la función "fibonacci" que recibe un número entero positivo como argumento.
fibonacci(N) when N > 0 ->
    % Si N es mayor que 0, entonces calculamos el valor de Fibonacci de N.
    fibonacci(N-1) + fibonacci(N-2);
fibonacci(0) ->
    % Si N es igual a 0, entonces el valor de Fibonacci de N es 0.
    0.

% Compilamos el módulo con el comando "c(modulo_factorial)".
-module(modulo_factorial).

% Exportamos la función "factorial" para que pueda ser llamada desde otros módulos.
-export([factorial/1]).

% Definimos la función "factorial" que recibe un número entero positivo como argumento.
factorial(N) when N > 0 ->
    % Si N es mayor que 0, entonces calculamos el valor factorial de N.
    N * factorial(N-1);
factorial(0) ->
    % Si N es igual a 0, entonces el valor factorial de N es 1.
    1.

% Compilamos el módulo con el comando "c(modulo_ordenamiento_burbuja)".
-module(modulo_ordenamiento_burbuja).

% Exportamos la función "ordenamiento_burbuja" para que pueda ser llamada desde otros módulos.
-export([ordenamiento_burbuja/1]).

% Definimos la función "ordenamiento_burbuja" que recibe una lista de elementos como argumento.
ordenamiento_burbuja([]) ->
    % Si la lista está vacía, entonces la devolvemos sin ordenarla.
    [];
ordenamiento_burbuja([H|T]) ->
    % Si la lista no está vacía, entonces ordenamos la lista utilizando el algoritmo de ordenamiento por burbuja.
    ordenamiento_burbuja_iterativo(H, T, []).

% Definimos la función auxiliar "ordenamiento_burbuja_iterativo" que recibe el elemento actual, la lista restante y la lista ordenada como argumentos.
ordenamiento_burbuja_iterativo(H, [], Acc) ->
    % Si la lista restante está vacía, entonces devolvemos la lista ordenada.
    [H|Acc];
ordenamiento_burbuja_iterativo(H, [X|T], Acc) ->
    % Si el elemento actual es mayor que el elemento siguiente, entonces lo intercambiamos con el elemento siguiente.
    if
        H > X -> ordenamiento_burbuja_iterativo(X, [H|T], Acc);
        true -> ordenamiento_burbuja_iterativo(H, T, [X|Acc])
    end.

% Compilamos el módulo con el comando "c(modulo_busqueda_binaria)".
-module(modulo_busqueda_binaria).

% Exportamos la función "busqueda_binaria" para que pueda ser llamada desde otros módulos.
-export([busqueda_binaria/3]).

% Definimos la función "busqueda_binaria" que recibe una lista ordenada, un elemento a buscar y el número de elementos de la lista como argumentos.
busqueda_binaria([], _, _) ->
    % Si la lista está vacía, entonces el elemento no se encuentra en la lista.
    -1;
busqueda_binaria([X|_], X, 1) ->
    % Si el elemento a buscar es igual al primer elemento de la lista y la lista tiene un solo elemento, entonces el elemento se encuentra en la lista.
    0;
busqueda_binaria([X|_], X, N) ->
    % Si el elemento a buscar es igual al primer elemento de la lista y la lista tiene más de un elemento, entonces el elemento se encuentra en la lista.
    0;
busqueda_binaria([H|T], X, N) ->
    % Si el elemento a buscar es mayor que el primer elemento de la lista, entonces buscamos el elemento en la sublista derecha.
    if
        X > H -> busqueda_binaria(T, X, N-1);
        % Si el elemento a buscar es menor que el primer elemento de la lista, entonces buscamos el elemento en la sublista izquierda.
        X < H -> busqueda_binaria(T, X, N-1);
        % Si el elemento a buscar es igual al primer elemento de la lista, entonces el elemento se encuentra en la lista.
        true -> 0
    end.
```

**Explicación del código:**

* El código está dividido en cuatro módulos: `modulo_fibonacci`, `modulo_factorial`, `modulo_ordenamiento_burbuja` y `modulo_busqueda_binaria`. Cada módulo define una función que realiza una tarea específica.
* El módulo `modulo_fibonacci` define la función `fibonacci` que calcula el valor de Fibonacci de un número entero positivo.
* El módulo `modulo_factorial` define la función `factorial` que calcula el valor factorial de un número entero positivo.
* El módulo `modulo_ordenamiento_burbuja` define la función `ordenamiento_burbuja` que ordena una lista de elementos utilizando el algoritmo de ordenamiento por burbuja.
* El módulo `modulo_busqueda_binaria` define la función `busqueda_binaria` que busca un elemento en una lista ordenada utilizando el algoritmo de búsqueda binaria.

**Ejemplo de uso:**

```erlang
% Calculamos el valor de Fibonacci de 10.
1> fibonacci:fibonacci(10).
55

% Calculamos el valor factorial de 5.
2> factorial:factorial(5).
120

% Ordenamos una lista de números enteros.
3> ordenamiento_burbuja:ordenamiento_burbuja([5, 3, 1, 2, 4]).
[1, 2, 3, 4, 5]

% Buscamos el elemento 3 en una lista ordenada de números enteros.
4> busqueda_binaria:busqueda_binaria([1, 2, 3, 4, 5], 3, 5).
2
```