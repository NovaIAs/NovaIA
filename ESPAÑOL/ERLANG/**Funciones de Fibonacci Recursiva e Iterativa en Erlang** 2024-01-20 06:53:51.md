```erlang
-module(fibonacci).
-export([fibonacci/1, fibonacci_iterative/1]).

% Función recursiva para calcular el número de Fibonacci de un número dado.
fibonacci(N) when N < 0 -> 
     exit({error, "El número no puede ser negativo"});
fibonacci(0) ->
     0;
fibonacci(1) ->
     1;
fibonacci(N) ->
     fibonacci(N - 1) + fibonacci(N - 2).

% Función iterativa para calcular el número de Fibonacci de un número dado.
fibonacci_iterative(N) when N < 0 ->
     exit({error, "El número no puede ser negativo"});
fibonacci_iterative(0) ->
     0;
fibonacci_iterative(1) ->
     1;
fibonacci_iterative(N) ->
     {A, B} = {0, 1},
     fibonacci_iterative(N, A, B).

% Función auxiliar para la función fibonacci_iterative.
fibonacci_iterative(N, A, B) when N == 1 ->
     B;
fibonacci_iterative(N, A, B) ->
     fibonacci_iterative(N - 1, B, A + B).
```

Explicación del código:

1. El módulo `fibonacci` define dos funciones: `fibonacci/1` y `fibonacci_iterative/1`.

2. La función `fibonacci/1` calcula el número de Fibonacci de un número dado `N` utilizando la recursión. Si `N` es menor que 0, se genera un error. Si `N` es 0 o 1, se devuelve el valor correspondiente. De lo contrario, se calcula el número de Fibonacci de `N` sumando los números de Fibonacci de `N - 1` y `N - 2`.

3. La función `fibonacci_iterative/1` calcula el número de Fibonacci de un número dado `N` utilizando la iteración. Si `N` es menor que 0, se genera un error. Si `N` es 0 o 1, se devuelve el valor correspondiente. De lo contrario, se utilizan dos variables, `A` y `B`, para almacenar los dos últimos números de Fibonacci calculados. La función llama a sí misma de forma recursiva hasta que `N` sea igual a 1, momento en el que devuelve el valor de `B`.

4. La función auxiliar `fibonacci_iterative/3` se utiliza para implementar la función `fibonacci_iterative/1`. Recibe tres argumentos: `N`, `A` y `B`. `N` es el número de Fibonacci que se quiere calcular, `A` es el último número de Fibonacci calculado y `B` es el penúltimo número de Fibonacci calculado. La función llama a sí misma de forma recursiva hasta que `N` sea igual a 1, momento en el que devuelve el valor de `B`. En cada llamada recursiva, se actualizan los valores de `A` y `B` para almacenar los dos últimos números de Fibonacci calculados.