```erlang

% Módulo principal
-module(codigo_complejo).

% Exportación de funciones
-export([iniciar/0, saludar/1]).

% Función de inicio
iniciar() ->
    io:fwrite("Bienvenido a Erlang~n"),
    saludarse("mundo").

% Función de saludo
saludarse(Nombre) ->
    io:fwrite("Hola, ~s!~n", [Nombre]),
    io:fwrite("Espero que estés teniendo un gran día.~n"),
    ok.

% Función para calcular el factorial de un número
factorial(N) when N >= 0 ->
    factorial(N, 1).

% Función auxiliar para calcular el factorial de un número
factorial(0, Resultado) ->
    Resultado;
factorial(N, Resultado) ->
    factorial(N-1, Resultado*N).

% Función para comprobar si un número es primo
primo(N) when N > 1 ->
    primo(N, 2).

% Función auxiliar para comprobar si un número es primo
primo(N, X) when X * X <= N ->
    if
        N rem X == 0 ->
            false;
        true ->
            primo(N, X+1)
    end;
primo(_, _) ->
    true.

% Función para encontrar el máximo común divisor de dos números
mcd(X, Y) ->
    mcd_aux(X, Y, min(X, Y)).

% Función auxiliar para encontrar el máximo común divisor de dos números
mcd_aux(_, 0, MCD) ->
    MCD;
mcd_aux(X, Y, MCD) ->
    mcd_aux(Y, X rem Y, MCD).

% Función para calcular la potencia de un número
potencia(Base, Exponente) when Exponente >= 0 ->
    potencia(Base, Exponente, 1).

% Función auxiliar para calcular la potencia de un número
potencia(_, 0, Resultado) ->
    Resultado;
potencia(Base, Exponente, Resultado) ->
    potencia(Base, Exponente-1, Resultado*Base).

```

Explicación del código:

1. El módulo `codigo_complejo` define las funciones principales del programa.

2. La función `iniciar/0` es el punto de entrada del programa. Llama a la función `saludarse/1` para saludar al usuario.

3. La función `saludarse/1` escribe un saludo personalizado al usuario.

4. La función `factorial/1` calcula el factorial de un número.

5. La función `primo/1` comprueba si un número es primo.

6. La función `mcd/2` calcula el máximo común divisor de dos números.

7. La función `potencia/2` calcula la potencia de un número.

8. La función `mcd_aux/3` es una función auxiliar para calcular el máximo común divisor de dos números.

9. La función `potencia/3` es una función auxiliar para calcular la potencia de un número.

10. Las funciones utilizan diversos patrones de coincidencia para manejar diferentes casos y datos de entrada.

11. El programa utiliza la función `io:fwrite/2` para escribir texto en la consola.