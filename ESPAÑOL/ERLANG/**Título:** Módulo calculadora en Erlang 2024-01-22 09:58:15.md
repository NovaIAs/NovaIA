```erlang
-module(calculadora).
-export([sumar/2, restar/2, multiplicar/2, dividir/2]).

% Función sumar
sumar(A, B) -> A + B.

% Función restar
restar(A, B) -> A - B.

% Función multiplicar
multiplicar(A, B) -> A * B.

% Función dividir
dividir(A, B) when B /= 0 -> A / B;
                      _ -> error('División por 0').
```

Explicación:

* El código define un módulo llamado `calculadora` que exporta cuatro funciones: `sumar`, `restar`, `multiplicar` y `dividir`.
* La función `sumar` toma dos argumentos, `A` y `B`, y devuelve su suma.
* La función `restar` toma dos argumentos, `A` y `B`, y devuelve su resta.
* La función `multiplicar` toma dos argumentos, `A` y `B`, y devuelve su producto.
* La función `dividir` toma dos argumentos, `A` y `B`, y devuelve su división. Si `B` es igual a 0, la función devuelve un error.