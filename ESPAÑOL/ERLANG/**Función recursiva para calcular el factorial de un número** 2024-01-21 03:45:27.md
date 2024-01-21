```erlang

-module(factorial).
-export([factorial/1]).

factorial(N) when N < 0 ->
    throw({error, "N must be non-negative"});
factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

```

Este código calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales a ese número. Por ejemplo, el factorial de 5 es 120 (5 x 4 x 3 x 2 x 1).

El código anterior utiliza una función recursiva, que es una función que se llama a sí misma. La función factorial toma un argumento, N, que es el número del que queremos calcular el factorial. Si N es menor que 0, la función lanza una excepción. Si N es 0, la función devuelve 1. De lo contrario, la función multiplica N por el factorial de N-1.

Este código es complejo porque utiliza una función recursiva. Las funciones recursivas pueden ser difíciles de entender y depurar, por lo que es importante utilizarlas con cuidado. Sin embargo, las funciones recursivas pueden ser muy útiles para resolver ciertos tipos de problemas. En este caso, la función recursiva se utiliza para calcular el factorial de un número.

Aquí hay un ejemplo de cómo se puede utilizar el código anterior:

```erlang

1> factorial(5).
120

```

Este código calcula el factorial de 5. La función factorial toma el argumento 5 y lo pasa a la función factorial/1. La función factorial/1 calcula el factorial de 5 y devuelve el resultado, que es 120.

Este código es complejo, pero es un ejemplo de cómo se pueden utilizar las funciones recursivas para resolver ciertos tipos de problemas.