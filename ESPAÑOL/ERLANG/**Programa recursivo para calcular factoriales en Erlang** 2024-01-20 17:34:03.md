```erlang

-module(mi_modulo).
-export([factorial/1]).

factorial(0) -> 1;
factorial(N) when N > 0 -> N * factorial(N-1).

```

Este código calcula el factorial de un número. El factorial de un número es el producto de todos los números enteros positivos menores o iguales que ese número. Por ejemplo, el factorial de 5 es 5 * 4 * 3 * 2 * 1 = 120.

La función `factorial/1` recibe un número entero como argumento y devuelve su factorial. La función utiliza la recursión para calcular el factorial. Si el número es 0, la función devuelve 1. Si el número es mayor que 0, la función multiplica el número por el factorial del número menos uno.

Por ejemplo, para calcular el factorial de 5, la función `factorial/1` llama a sí misma con el argumento 4. Esto devuelve el factorial de 4, que es 24. La función `factorial/1` entonces multiplica 5 por 24 para obtener el factorial de 5, que es 120.

Este código es un ejemplo de cómo se puede utilizar la recursión para escribir código en Erlang. La recursión es una técnica de programación que permite a una función llamarse a sí misma. Esto puede ser útil para resolver problemas que tienen una estructura recursiva, como el cálculo del factorial de un número.