```erlang
-module(mi_modulo).
-export([funcion_principal/0]).

funcion_principal() ->
    %Creamos una lista con los números del 1 al 10
    Lista_numeros = lists:seq(1, 10),
    
    %Imprimimos la lista en pantalla
    io:fwrite("Lista original: ~p~n", [Lista_numeros]),
    
    %Filtramos los números pares de la lista
    Lista_pares = [X || X <- Lista_numeros, X rem 2 == 0],
    
    %Imprimimos la lista de números pares en pantalla
    io:fwrite("Lista de números pares: ~p~n", [Lista_pares]),
    
    %Calculamos la suma de los números de la lista
    Suma_numeros = lists:sum(Lista_numeros),
    
    %Imprimimos la suma de los números en pantalla
    io:fwrite("Suma de los números: ~p~n", [Suma_numeros]),
    
    %Creamos un mapa con los números de la lista como claves y sus cuadrados como valores
    Mapa_numeros = maps:from_list([{X, X*X} || X <- Lista_numeros]),
    
    %Imprimimos el mapa en pantalla
    io:fwrite("Mapa de números y sus cuadrados: ~p~n", [Mapa_numeros]).
```

Explicación del código:

* El primer módulo es mi_modulo. Este es el nombre del módulo que define la función principal.
* El módulo exporta una función, funcion_principal/0. Esto significa que esta función puede ser llamada desde otros módulos.
* La función funcion_principal() hace lo siguiente:
    * Crea una lista con los números del 1 al 10 usando lists:seq/2.
    * Imprime la lista en pantalla usando io:fwrite/2.
    * Filtra los números pares de la lista usando una comprensión de listas.
    * Imprime la lista de números pares en pantalla.
    * Calcula la suma de los números de la lista usando lists:sum/1.
    * Imprime la suma de los números en pantalla.
    * Crea un mapa con los números de la lista como claves y sus cuadrados como valores usando maps:from_list/1.
    * Imprime el mapa en pantalla.