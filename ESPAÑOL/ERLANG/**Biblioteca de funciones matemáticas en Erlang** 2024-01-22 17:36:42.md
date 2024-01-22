```erlang
% Define una función para saludar a alguien.
-module(saludar).
-export([saludar/1]).

% La función saludar toma un nombre como argumento y devuelve un mensaje de saludo.
saludar(Nombre) ->
    "Hola, " ++ Nombre ++ "!".

% Define una función para sumar dos números.
-module(sumar).
-export([sumar/2]).

% La función sumar toma dos números como argumentos y devuelve su suma.
sumar(A, B) ->
    A + B.

% Define una función para restar dos números.
-module(restar).
-export([restar/2]).

% La función restar toma dos números como argumentos y devuelve su diferencia.
restar(A, B) ->
    A - B.

% Define una función para multiplicar dos números.
-module(multiplicar).
-export([multiplicar/2]).

% La función multiplicar toma dos números como argumentos y devuelve su producto.
multiplicar(A, B) ->
    A * B.

% Define una función para dividir dos números.
-module(dividir).
-export([dividir/2]).

% La función dividir toma dos números como argumentos y devuelve su cociente.
dividir(A, B) ->
    A / B.

% Define una función para calcular el factorial de un número.
-module(factorial).
-export([factorial/1]).

% La función factorial toma un número como argumento y devuelve su factorial.
factorial(0) ->
    1;
factorial(N) ->
    N * factorial(N-1).

% Define una función para calcular el máximo común divisor de dos números.
-module(mcd).
-export([mcd/2]).

% La función mcd toma dos números como argumentos y devuelve su máximo común divisor.
mcd(A, B) ->
    case B of
        0 ->
            A;
        _ ->
            mcd(B, A rem B)
    end.

% Define una función para calcular el mínimo común múltiplo de dos números.
-module(mcm).
-export([mcm/2]).

% La función mcm toma dos números como argumentos y devuelve su mínimo común múltiplo.
mcm(A, B) ->
    A * B div mcd(A, B).

% Define una función para generar una lista de números primos.
-module(primos).
-export([primos/1]).

% La función primos toma un número como argumento y devuelve una lista de los números primos hasta ese número.
primos(N) ->
    primos(N, 2, []).

primos(N, I, Primos) ->
    case I > N of
        true ->
            Primos;
        false ->
            case I rem 2 == 0 andalso I /= 2 ->
                primos(N, I+1, Primos);
            true ->
                case lists:any(fun(X) -> I rem X == 0 end, Primos) ->
                    primos(N, I+2, Primos);
                true ->
                    primos(N, I+2, [I|Primos])
            end
    end.

% Define una función para generar una lista de números de Fibonacci.
-module(fibonacci).
-export([fibonacci/1]).

% La función fibonacci toma un número como argumento y devuelve una lista de los números de Fibonacci hasta ese número.
fibonacci(N) ->
    fibonacci(N, 0, 1, []).

fibonacci(N, A, B, Fibo) ->
    case N > 0 of
        true ->
            fibonacci(N-1, B, A+B, [A+B|Fibo]);
        false ->
            Fibo
    end.
```

Explicación:

* El código define varias funciones, cada una de las cuales realiza una operación matemática diferente. Por ejemplo, la función `sumar` toma dos números como argumentos y devuelve su suma.


* Las funciones se definen utilizando el módulo `-module` y la lista de funciones que exporta el módulo utilizando `-export`.


* Las funciones toman argumentos utilizando la sintaxis `(Argumentos) ->`, y devuelven un valor utilizando `-> Valor`.


* El código utiliza la coincidencia de patrones para determinar qué función se debe llamar en función de los argumentos que se le pasan. Por ejemplo, la función `primos` toma un número como argumento y devuelve una lista de los números primos hasta ese número. La función utiliza la coincidencia de patrones para determinar si el número es primo o no, y luego llama a la función `primos` de forma recursiva para generar una lista de los números primos hasta ese número.


* El código también utiliza la recursividad para generar una lista de números de Fibonacci. La función `fibonacci` toma un número como argumento y devuelve una lista de los números de Fibonacci hasta ese número. La función llama a sí misma de forma recursiva para generar una lista de los números de Fibonacci hasta ese número.