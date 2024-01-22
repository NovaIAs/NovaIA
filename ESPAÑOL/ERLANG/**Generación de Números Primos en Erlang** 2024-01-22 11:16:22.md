```erlang
-module(numeros_primos).
-export([es_primo/1, generador_primos/0]).

es_primo(N) -> es_primo(N, 2).

es_primo(N, D) when D * D > N -> true;
es_primo(N, D) when rem(N, D) =:= 0 -> false;
es_primo(N, D) -> es_primo(N, D+1).

generador_primos() -> generador_primos(3).

generador_primos(N) ->
  if es_primo(N) ->
    io:format("~p~n", [N]),
    generador_primos(N+2);
  true -> generador_primos(N+2)
  end.
```

Este código en Erlang genera números primos infinitamente a partir de 3. Explicaré cada parte del código en detalle a continuación:

1. Módulo y función de exportación:

   - El módulo se llama `numeros_primos` y exporta dos funciones: `es_primo/1` y `generador_primos/0`.

2. Función `es_primo/1`:

   - Esta función comprueba si un número dado es primo.
   - Recibe un argumento `N`, que es el número a comprobar.
   - Utiliza una función recursiva interna, `es_primo(N, D)`, donde `D` es el divisor actual que se está utilizando para comprobar si `N` es primo.
   - La base del caso: cuando `D * D` es mayor que `N`, significa que se han comprobado todos los divisores hasta la raíz cuadrada de `N`, y si ningún divisor ha dividido a `N` hasta ahora, entonces `N` es primo.
   - Caso recursivo: Si `rem(N, D)` es igual a 0, significa que `D` divide a `N`, por lo que `N` no es primo.
   - Caso recursivo: Si `rem(N, D)` no es igual a 0, significa que `D` no divide a `N`, por lo que `D` se incrementa en 1 y se comprueba el siguiente divisor.

3. Función `generador_primos/0`:

   - Esta función genera números primos infinitamente a partir de 3.
   - Llama a la función `es_primo/1` para comprobar si el número actual `N` es primo.
   - Si `N` es primo, lo imprime y luego llama a la función de forma recursiva con `N+2`. Esto garantiza que sólo se comprueben los números impares, ya que los números pares mayores que 2 no pueden ser primos.
   - Si `N` no es primo, se llama a la función de forma recursiva con `N+2` para comprobar el siguiente número impar.

Este código es un ejemplo de programación recursiva en Erlang, y demuestra cómo se pueden generar números primos de forma eficiente utilizando la concurrencia y la programación funcional.