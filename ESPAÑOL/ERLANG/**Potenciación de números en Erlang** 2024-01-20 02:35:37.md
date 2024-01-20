```erlang
-module(potencia).
-export([potencia/2]).

potencia(Base, Exponente) ->
    if
        Exponente == 0 -> 1;
        Exponente < 0 -> 1 / potencia(Base, -Exponente);
        true -> Base * potencia(Base, Exponente - 1)
    end.
```

Explicación:

* El módulo `potencia` define una función `potencia/2` que calcula la potencia de una base elevada a un exponente.
* La función `potencia/2` tiene dos argumentos: `Base` y `Exponente`.
* La función `potencia/2` utiliza recursividad para calcular la potencia.
* Si el exponente es 0, la función devuelve 1.
* Si el exponente es negativo, la función devuelve 1 dividido entre la potencia de la base elevada al exponente negativo.
* Si el exponente es positivo, la función devuelve la base multiplicada por la potencia de la base elevada al exponente menos 1.

Ejemplo:

```erlang
1> potencia(2, 3).
8
2> potencia(5, -2).
0.04
```